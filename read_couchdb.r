#!/usr/bin/Rscript --slave

library(tidyverse, quietly=TRUE)
library(jsonlite, quietly=TRUE)
library(sofa, quietly=TRUE)
library(yaml, quietly=TRUE)
library(stringr, quietly=TRUE)

args = commandArgs(TRUE)

cfg_file = "config.yml"
if (length(args>0))
  cfg_file = args[1]

cfg = yaml.load_file(cfg_file)

data_path = cfg$data$path
suffix = cfg$data$suffix
txt_path = cfg$data$txt_path
files_path = cfg$data$files_path

# read couchDB
#=============================
cdb = do.call(Cushion$new, cfg$couchdb)

dbs = db_list(cdb)

# read users
userids = cdb %>% 
  db_alldocs("_users", include_docs=TRUE, as = "json") %>% 
  fromJSON() %>% 
  .$rows %>% 
  .$doc %>% 
  filter(name != "root", !is.na(name)) %>% 
  .$name %>% 
  keep(~.x %in% dbs)

# read papers
papers = cdb %>% 
  db_alldocs(str_c("papers", suffix), include_docs=TRUE, as = "json") %>% 
  fromJSON() %>% 
  .$rows %>% 
  .$doc %>%
  as_tibble() %>% 
  select(paper_id, filename)

# Read topics
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


# Read user topics
user_topics = NULL
for (i in seq_along(userids)) {
  user = userids[[i]]
  if (user %in% db_list(cdb)) {
    
    tc_ids = tryCatch({
      cdb %>% 
        doc_get(user, 
                "preferred_topics") %>% 
        .$topic_cluster_ids %>% 
        unlist() %>% 
        unique()
      }, 
        error = function(err) NULL
    )
    if (length(tc_ids)>0) {
      user_topics = user_topics %>% 
        bind_rows(data_frame(user = user, topic_cluster_id = tc_ids))
    }
  }
}

user_topics = user_topics %>% 
  left_join(topic_clusters, by = "topic_cluster_id") %>% 
  select(-topic_cluster_id) %>% 
  mutate(point = 1) %>% 
  complete(user = userids, topic_id = topic_ids, fill = list(point=0))

# read paper filenames
filenames = readLines(files_path) %>% 
  tools::file_path_sans_ext() %>% 
  basename()

# generate random user likes
if ("simu" %in% names(cfg)) {
  set.seed(cfg$simu$seed)
  n_likes = cfg$simu$n_likes
  
  users_ = sample(userids, n_likes, replace = TRUE)
  papers_ = sample(filenames, n_likes, replace = TRUE)
  userlikes = data_frame(user = users_, 
                         filename = papers_,
                         ctr_user_id = match(users_, userids)-1, 
                         ctr_paper_id = match(papers_, filenames)-1) %>%  # NOTE: ctr ids start at 0
    left_join(papers, by = "filename")
} else {
  # Read user likes
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
    distinct(user, filename)
  
  # join with ctr ids
  userlikes = userlikes %>% 
    mutate(ctr_user_id = match(user, userids)-1,
           ctr_paper_id = match(filename, filenames)-1) # NOTE: ctr ids start at 0
}

# save userids file
fn = file.path(data_path, 'userids.dat')
cat("writing", fn, "\n")

userids %>% 
  writeLines(fn)

# save users file
fn = file.path(data_path, 'users.dat')
cat("writing", fn, "\n")

userlikes %>% 
  filter(!is.na(ctr_paper_id)) %>% 
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
  spread(topic_id, point) %>%
  slice(match(userids, user)) %>% 
  select(-user) %>% 
  as_tibble()

theta_u %>% 
  write_delim(fn, delim =" ", col_names = FALSE)
