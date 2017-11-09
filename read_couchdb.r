#!/usr/bin/Rscript --slave

t_start = Sys.time()

source("common.r")

# read couchDB
#=============================
cdb = do.call(Cushion$new, couchdb_args)

dbs = db_list(cdb)

# read users
cat("reading couchdb users\n")

userids = cdb %>% 
  db_alldocs("_users", include_docs=TRUE, as = "json") %>% 
  fromJSON() %>% 
  .$rows %>% 
  .$doc %>% 
  filter(name != "root", !is.na(name)) %>% 
  .$name %>% 
  keep(~.x %in% dbs)

# read papers
cat("reading couchdb papers\n")

papers = cdb %>% 
  db_alldocs(str_c("papers", suffix), include_docs=TRUE, as = "json") %>% 
  fromJSON() %>% 
  .$rows %>% 
  .$doc %>%
  as_tibble() %>% 
  select(paper_id, filename)

# Read topics
cat("reading couchdb topics\n")

topics = cdb %>%
  db_alldocs(str_c("topics", suffix), include_docs=TRUE, as = "json") %>%
  fromJSON() %>%
  .$rows %>%
  .$doc %>%
  as_tibble() %>% 
  select(topic_id, topic_cluster_ids)

topic_ids = topics$topic_id %>% 
  sort()

topic_clusters = topics %>% 
  filter(map_lgl(topic_cluster_ids, ~length(.x)>0)) %>% 
  unnest() %>% 
  rename(topic_cluster_id = topic_cluster_ids)

# Read user tables
cat("reading couchdb user tables\n")

user_tables = list()
for (i in seq_along(userids)) {
  user = userids[[i]]
  user_tables[[user]] = cdb %>% 
    db_alldocs(user, include_docs=TRUE, as = "json") %>% 
    fromJSON(simplifyDataFrame = FALSE) %>% 
    .$rows %>% 
    map("doc")
}

user_tables = user_tables %>% 
  map(~setNames(.x, map_chr(.x, "_id")))

# user topics
user_topics = user_tables %>% 
  map("preferred_topics") %>% 
  map(safely(function(x) x$topic_cluster_ids)) %>% 
  map("result") %>% 
  map(~list(topic_cluster_id = unlist(.x))) %>% 
  keep(~length(.x$topic_cluster_id)>0) %>% 
  bind_rows(.id = "user")

user_topics = user_topics %>% 
  left_join(topic_clusters, by = "topic_cluster_id") %>% 
  select(-topic_cluster_id) %>% 
  distinct() %>% 
  mutate(weight = 1) %>% # each prefered topic gets the same weight
  complete(user = userids, topic_id = topic_ids, fill = list(weight=0)) %>% 
  mutate(ctr_user_id = match(user, userids)-1) # NOTE: ctr ids start at 0

# user bookmarks
bookmarks = user_tables %>% 
  map("data") %>% 
  map(safely(function(x) x$bookmarks)) %>% 
  map("result") %>% 
  map(~list(paper_id = unlist(.x))) %>% 
  keep(~length(.x$paper_id)>0) %>% 
  bind_rows(.id = "user")

# read paper filenames
filenames = readLines(files_path) %>% 
  tools::file_path_sans_ext() %>% 
  basename()

if (!is.na(simu_seed)) {
  # generate random user likes
  cat("generating random user likes\n")
  
  userlikes = sample_userlikes(userids, filenames,
                               n_likes = simu_n_likes,
                               seed = simu_seed)
  
  userlikes = userlikes %>% 
    mutate(ctr_user_id = match(users_, userids)-1, 
           ctr_paper_id = match(papers_, filenames)-1) %>% # NOTE: ctr ids start at 0
    left_join(papers, by = "filename")
} else {
  # Read user likes
  cat("reading couchdb user likes\n")
  
  parse_likes = function(x) {
    data_frame(user = x$`_id`,
               paper_id = names(x$likes),
               time = map_chr(x$likes, "time"))
  }
  
  userlikes = cdb %>% 
    db_alldocs("userlikes_reversed", include_docs=TRUE, as = "json") %>% 
    rjson::fromJSON() %>% 
    .$rows %>% 
    map("doc") %>% 
    keep(~has_name(.x, "likes")) %>% 
    keep(~length(.x$likes)>0) %>% 
    map_df(parse_likes) %>% 
    inner_join(papers %>% 
                 mutate(paper_id = as.character(paper_id)), 
               by = "paper_id") %>% 
    mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%S")) %>% 
    arrange(desc(time)) %>% 
    distinct(user, paper_id, filename)
  
  # join with ctr ids
  userlikes = userlikes %>% 
    mutate(ctr_user_id = match(user, userids)-1,
           ctr_paper_id = match(filename, filenames)-1) # NOTE: ctr ids start at 0
}

userlikes = userlikes %>% 
  filter(!is.na(ctr_user_id), !is.na(ctr_paper_id))

# Write .dat files
#===================

# save userids file
fn = file.path(data_path, 'userids.dat')
cat("writing", fn, "\n")

userids %>% 
  writeLines(fn)

# save users file
fn = file.path(data_path, 'users.dat')
cat("writing", fn, "\n")

userlikes %>% 
  select(ctr_user_id, ctr_paper_id) %>% 
  group_by(ctr_user_id) %>% 
  nest(ctr_paper_id, .key = "ctr_paper_ids") %>% 
  mutate(n = map_int(ctr_paper_ids, nrow),
         ctr_paper_ids = map_chr(ctr_paper_ids, ~str_c(.x$ctr_paper_id, collapse=" "))) %>% 
  complete(ctr_user_id = seq_along(userids)-1, 
           fill=list(ctr_paper_ids = "", n=0)) %>% 
  transmute(out = str_c(n, ctr_paper_ids, sep=" ")) %>% 
  .$out %>% 
  writeLines(fn)

# save items file
fn = file.path(data_path, 'items.dat')
cat("writing", fn, "\n")

userlikes %>% 
  select(ctr_user_id, ctr_paper_id) %>% 
  group_by(ctr_paper_id) %>% 
  nest(ctr_user_id, .key = "ctr_user_ids") %>% 
  mutate(n = map_int(ctr_user_ids, nrow),
         ctr_user_ids = map_chr(ctr_user_ids, ~str_c(.x$ctr_user_id, collapse=" "))) %>% 
  complete(ctr_paper_id = seq_along(filenames)-1, 
           fill=list(ctr_user_ids = "", n=0)) %>% 
  transmute(out = str_c(n, ctr_user_ids, sep=" ")) %>% 
  .$out %>% 
  writeLines(fn)

# write user topics
#=====================

fn = file.path(data_path, 'theta_u.dat')
cat("writing", fn, "\n")

theta_u = user_topics %>% 
  spread(topic_id, weight) %>%
  arrange(ctr_user_id) %>% 
  select(-user, -ctr_user_id) %>% 
  as_tibble()

theta_u %>% 
  write_delim(fn, delim =" ", col_names = FALSE)

# Compute trending
#=======================
fn = file.path(data_path, 'trending.csv')
cat("writing", fn, "\n")

trending = bookmarks %>% 
  mutate(weight = trending_bookmark_weight) %>% 
  bind_rows(userlikes %>% 
              filter(!is.na(ctr_user_id), !is.na(ctr_paper_id)) %>% 
              select(user, paper_id) %>% 
              mutate(weight = trending_like_weight)) %>% 
  group_by(paper_id) %>% 
  summarise(weight = sum(weight)) %>% 
  ungroup() %>% 
  arrange(desc(weight)) %>% 
  write_csv(fn)

# elapsed time
Sys.time()-t_start
