#!/usr/bin/Rscript --slave

t_start = Sys.time()

source("common.r")

# read couchDB
#=============================

cdb = do.call(Cushion$new, couchdb_args)

# read papers
cat("reading couchdb papers\n")

papers = cdb %>% 
  db_alldocs(str_c("papers", suffix), include_docs=TRUE, as = "json") %>% 
  fromJSON() %>% 
  .$rows %>% 
  .$doc %>%
  as_tibble() %>% 
  select(paper_id, filename)

# read userids
userids = readLines(file.path(data_path, "userids.dat"))

# read filenames
filenames = readLines(files_path) %>% 
  tools::file_path_sans_ext() %>% 
  basename()

cat("reading couchdb papers\n")

if (!is.na(simu_seed)) {
  # generate random user likes
  cat("generating random user likes\n")
  
  userlikes = sample_userlikes(userids, filenames,
                               n_likes = simu_n_likes,
                               seed = simu_seed)
  
  userlikes = userlikes %>% 
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
    mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%S")) %>% 
    arrange(desc(time)) %>% 
    distinct(user, paper_id)
}

# Read user recommendations and dismissed
cat("reading couchdb user recommendations and dismissed\n")

reco = list()
for (i in seq_along(userids)) {
  user = userids[[i]]
  if (user %in% db_list(cdb)) {
    
    doc = tryCatch(doc_get(cdb, user, "recommendations")[-(1:2)], 
                   error = function(err) NULL)
    
    if (!is.null(doc)) 
      reco[[user]] = doc
  }
}

# read CTR output
#==================

# read U
U = read_delim(file.path(ctr_output_path, "final-U.dat"), 
               delim = " ", col_names = FALSE,
               col_types = cols(.default="d")) %>% 
  select(-ncol(.)) %>% 
  as.matrix()

# read V
V = read_delim(file.path(ctr_output_path, "final-V.dat"), 
               delim = " ", col_names = FALSE,
               col_types = cols(.default="d")) %>% 
  select(-ncol(.)) %>% 
  as.matrix()

# compute scores
#==================
cat("computing CTR user/paper scores\n")

scores = as_tibble(U %*% t(V))

colnames(scores) = filenames

scores = scores %>%
  mutate(user = userids) %>%
  gather(filename, score, -user) %>%
  left_join(papers, by = "filename") 

# Write recommendations
#=============================
cat("writing recommendations to couchdb\n")

userlikes_split = userlikes %>% 
  split(.$user)

# # clear dismissed
# reco = reco %>% map(~.x[c("papers")]) %>% 
#   map(~keep(.x, ~length(.x)>0))

reco_new = scores %>% 
  group_by(user) %>% 
  filter( !(paper_id %in% unique(c(userlikes_split[[user[1]]][["paper_id"]],
                                   unlist(reco[[user[1]]][["dismissed"]]))) ) ) %>%  # remove liked and dismissed items
  arrange(desc(score)) %>% 
  slice(seq_len(reco_n_top)) %>% 
  nest(paper_id, score, .key = papers) %>% 
  left_join(reco %>% 
              { data_frame(user = names(.), doc = .) }, 
            by = "user") %>% 
  mutate(doc = map2(doc, papers, function(x,y) { 
    x$papers = y 
    return(x) 
  })) %>% 
  select(-papers) %>% 
  { setNames(.$doc, .$user) }

for (user in names(reco_new)) {
  if (!(user %in% db_list(cdb))) {
    cdb %>% db_create(user)
  }
  if (!is.na(couch_revs_limit)) {
    # set revisions limit
    cdb %>% db_revs_limit(user, couch_revs_limit)
  }
  # request compaction
  cdb %>% db_compact(user)
  
  rev = tryCatch(
    cdb %>% 
      db_revisions(user, "recommendations"), 
    error = function(err) NULL
  )
  
  doc = reco_new[[user]] %>% 
    toJSON(auto_unbox=TRUE) %>% 
    str_extract("\\{.+\\}")
  
  if (is.null(rev)) {
    cdb %>% doc_create(user, doc, "recommendations")
  } else {
    cdb %>% doc_update(user, doc, "recommendations", rev[1])
  }
}


# Write trending
#=============================
cat("writing trending papers to couchdb\n")

trending_ids = read_csv(file.path(data_path, "trending.csv")) %>% 
  arrange(desc(points)) %>% 
  slice(seq_len(trending_n_top)) %>% 
  .$paper_id

trending = trending_ids %>% 
  setNames(trending_ids) %>% 
  list() %>% 
  setNames(trending_field)
 
rev = tryCatch(
  cdb %>% 
    db_revisions(trending_dbname, trending_docid), 
  error = function(err) NULL
)

doc = trending %>% 
  toJSON() %>% 
  str_extract("\\{.+\\}")

if (is.null(rev)) {
  cdb %>% doc_create(trending_dbname, doc, trending_docid)
} else {
  cdb %>% doc_update(trending_dbname, doc, trending_docid, rev[1])
}

if (!is.na(couch_revs_limit)) {
  # set revisions limit
  cdb %>% db_revs_limit(trending_dbname, couch_revs_limit)
}
# request compaction
cdb %>% db_compact(trending_dbname)

# elapsed time
Sys.time()-t_start
