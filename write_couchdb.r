#!/usr/bin/Rscript --slave

library(tidyverse)
library(stringr)
library(jsonlite)
library(sofa)
library(yaml)

args = commandArgs(TRUE)

cfg_file = "config.yml"
if (length(args>0))
  cfg_file = args[1]

cfg = yaml.load_file(cfg_file)

data_path = cfg$data$path
suffix = cfg$data$suffix
txt_path = cfg$data$txt_path
files_path = cfg$data$files_path
output_path = cfg$ctr$output_path

# read couchDB
#=============================

cdb = do.call(Cushion$new, cfg$couchdb)

# read papers
papers = cdb %>% 
  db_alldocs(str_c("papers", suffix), include_docs=TRUE, as = "json") %>% 
  fromJSON() %>% 
  .$rows %>% 
  .$doc %>%
  as_tibble() %>% 
  select(paper_id, filename)

# read userids
userids = readLines(file.path(data_path, "userids.dat"))

# read paper ids
filenames = readLines(files_path) %>% 
  tools::file_path_sans_ext() %>% 
  basename()

# generate random user likes
if ("simu" %in% names(cfg)) {
  set.seed(cfg$simu$seed)
  n_likes = cfg$simu$n_likes
  set.seed(2017)
  n_likes = 100
  
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

# read CTR output
#==================

# read U
U = read_delim(file.path(output_path, "final-U.dat"), 
               delim = " ", col_names = FALSE,
               col_types = cols(.default="d")) %>% 
  select(-ncol(.)) %>% 
  as.matrix()

# read V
V = read_delim(file.path(output_path, "final-V.dat"), 
               delim = " ", col_names = FALSE,
               col_types = cols(.default="d")) %>% 
  select(-ncol(.)) %>% 
  as.matrix()

# compute scores
#==================
scores = as_tibble(U %*% t(V))

colnames(scores) = filenames

scores = scores %>% 
  mutate(user = userids) %>% 
  gather(filename, score, -user) %>% 
  left_join(papers, by = "filename")

# Write recommendations
#=============================

userlikes_split = userlikes %>% 
  split(.$user)

n_top = cfg$reco$n_top

reco = scores %>% 
  group_by(user) %>% 
  arrange(desc(score)) %>% 
  slice(seq_len(n_top)) %>% 
  filter( !(filename %in% userlikes_split[[user[1]]][["filename"]]) ) %>%  # remove liked items
  nest(paper_id, score, .key = papers) %>% 
  {split(select(., papers), .$user)}

for (i in seq_along(userids)) {
  user = userids[[i]]
  if (!user %in% db_list(cdb))
    cdb %>% db_create(user)
  
  rev = tryCatch(
    cdb %>% 
      db_revisions(user, "recommendations"), 
    error = function(err) NULL
  )
  
  doc = reco[[user]] %>% 
    toJSON() %>% 
    str_extract("\\{.+\\}")
  
  if (is.null(rev))
    cdb %>% doc_create(user, doc, "recommendations")
  else
    cdb %>% doc_update(user, doc, "recommendations", rev[1])
}
