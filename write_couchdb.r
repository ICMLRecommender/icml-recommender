#!/usr/bin/Rscript --slave

require(tidyr)
require(dplyr)
require(jsonlite)
require(readr)
require(sofa)
require(yaml)

args = commandArgs(TRUE)

cfg_file = "config.yml"
if (length(args>0))
  cfg_file = args[1]

cfg = yaml.load_file(cfg_file)

data_path = cfg$data$path
txt_path = cfg$pdfconversion$txt_path
output_path = cfg$ctr$output_path

# read couchDB
#=============================
cdb = do.call(Cushion$new, cfg$couchdb)

# read users
users = readLines(file.path(data_path, "userids.dat"))

# read paper ids
mlr_paper_ids = readLines(file.path(txt_path, "files.dat")) %>% 
  tools::file_path_sans_ext() %>% 
  basename()

# generate random user likes
if ("simu" %in% names(cfg)) {
  set.seed(cfg$simu$seed)
  n_likes = cfg$simu$n_likes
  set.seed(2017)
  n_likes = 100
  
  users_ = sample(users, n_likes, replace = TRUE)
  papers_ = sample(mlr_paper_ids, n_likes, replace = TRUE)
  userlikes = data_frame(user = users_, 
                         mlr_paper_id = papers_,
                         ctr_user_id = match(users_, users)-1, 
                         ctr_paper_id = match(papers_, mlr_paper_ids)-1) # NOTE: ctr ids start at 0
} else {
  # Read user likes
  userlikes = cdb %>% 
    db_alldocs("userlikes", include_docs=TRUE, as = "json") %>% 
    fromJSON() %>% 
    .$rows %>% 
    .$doc
  
  userlikes$likes = userlikes$likes %>% 
    gather(user, time)
  userlikes$likes$time = userlikes$likes$time[[1]]
  userlikes$likes = userlikes$likes %>% 
    split(f=seq_len(nrow(userlikes$likes)))
  
  userlikes = userlikes %>% 
    unnest() %>% 
    select(user, mlr_paper_id = `_id`, time) %>% 
    mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%S")) %>% 
    arrange(desc(time)) %>% 
    distinct()
  
  # join with ctr ids
  userlikes = userlikes %>% 
    mutate(ctr_user_id = match(user, users)-1,
           ctr_paper_id = match(mlr_paper_id, mlr_paper_ids)-1) # NOTE: ctr ids start at 0
  
}

# read CTR output
#==================

# read U
U = read_delim(file.path(output_path, "final-U.dat"), 
               delim = " ", col_names = FALSE,
               col_types = cols(.default="d", X51=col_skip())) %>% 
  as.matrix()

# read V
V = read_delim(file.path(output_path, "final-V.dat"), 
               delim = " ", col_names = FALSE,
               col_types = cols(.default="d", X51=col_skip())) %>% 
  as.matrix()

# compute scores
#==================
scores = tbl_df(U %*% t(V))

colnames(scores) = mlr_paper_ids

scores = scores %>% 
  mutate(user = users) %>% 
  gather(mlr_paper_id, score, -user)

# Write recommendations
#=============================

userlikes_split = userlikes %>% 
  split(., .$user)

n_top = cfg$reco$n_top

reco = scores %>% 
  group_by(user) %>% 
  arrange(desc(score)) %>% 
  slice(seq_len(n_top)) %>% 
  filter( !(mlr_paper_id %in% userlikes_split[[user[1]]]$mlr_paper_id) ) %>%  # remove liked items
  nest(mlr_paper_id, .key = recommendations) %>% 
  mutate(recommendations = lapply(recommendations, function(x) unlist(x, use.names = FALSE)))

reco = reco %>% 
  select(recommendations) %>% 
  split(., reco$user) %>% 
  lapply(function(x) list(papers = x$recommendations[[1]]))

for (i in seq_along(users)) {
  user = users[[i]]
  if (!user %in% db_list(cdb))
    cdb %>% db_create(user)
  
  rev = tryCatch(
    cdb %>% 
      db_revisions(user, "recommendations"), 
    error = function(err) NULL
  )
  
  if (is.null(rev))
    cdb %>% doc_create(user, reco[[user]], "recommendations")
  else
    cdb %>% doc_update(user, reco[[user]], "recommendations", rev)
  
}
