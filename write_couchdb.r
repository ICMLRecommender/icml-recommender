#!/usr/bin/Rscript --slave

library(tidyverse, quietly=TRUE)
library(stringr, quietly=TRUE)
library(jsonlite, quietly=TRUE)
library(sofa, quietly=TRUE)
library(yaml, quietly=TRUE)

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
    mutate(time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%S")) %>% 
    arrange(desc(time)) %>% 
    distinct(user, paper_id)
}

# Read user recommendations and dismissed
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

# Compute cold-start scores
#====================================
alpha_u_smooth = cfg$ctr$alpha_u_smooth
alpha_v_smooth = cfg$ctr$alpha_v_smooth

theta_u = read_delim(file.path(data_path, "theta_u.dat"), 
                     delim=" ", col_names = FALSE,
                     col_types = cols(.default = "d"))

theta_u = (theta_u + alpha_u_smooth) %>% 
  sweep(1, rowSums(.), "/") %>% 
  as.matrix()

theta_v = read_delim(file.path(lda_output_path, "final.gamma"), 
                     delim=" ", col_names = FALSE,
                     col_types = cols(.default = "d"))

theta_v = (theta_v + alpha_v_smooth) %>% 
  sweep(1, rowSums(.), "/") %>% 
  as.matrix()

scores_coldstart = (theta_u %*% t(theta_v)) %>% 
  as_tibble()

colnames(scores_coldstart) = filenames

scores_coldstart = scores_coldstart %>% 
  mutate(user = userids) %>% 
  anti_join(userlikes, by = "user") %>% # only users without likes
  gather(filename, score, -user) %>% 
  left_join(papers, by = "filename")

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
  semi_join(userlikes, by="user") %>% # only users with at least one like
  gather(filename, score, -user) %>% 
  left_join(papers, by = "filename") %>% 
  bind_rows(scores_coldstart)


# Write recommendations
#=============================

userlikes_split = userlikes %>% 
  split(.$user)

n_top = cfg$reco$n_top

reco_new = scores %>% 
  group_by(user) %>% 
  filter( !(paper_id %in% unique(c(userlikes_split[[user[1]]][["paper_id"]],
                                   unlist(reco[[user[1]]][["dismissed"]]),
                                   unlist(reco[[user[1]]][["latestdismissed"]])))) ) %>%  # remove liked and dismissed items
  arrange(desc(score)) %>% 
  slice(seq_len(n_top)) %>% 
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
  if (!user %in% db_list(cdb))
    cdb %>% db_create(user)
  
  rev = tryCatch(
    cdb %>% 
      db_revisions(user, "recommendations"), 
    error = function(err) NULL
  )
  
  doc = reco_new[[user]] %>% 
    toJSON() %>% 
    str_extract("\\{.+\\}")
  
  if (is.null(rev)) {
    cdb %>% doc_create(user, doc, "recommendations")
  } else {
    cdb %>% doc_update(user, doc, "recommendations", rev[1])
  }
}


# Write trending
#=============================

trending_dbname = cfg$trending$dbname
trending_docid = cfg$trending$docid
trending_field = cfg$trending$field
trending_n_top = cfg$trending$n_top

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
