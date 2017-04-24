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

# read couchDB
#=============================
cdb = do.call(Cushion$new, cfg$couchdb)

db_list(cdb)

# read users
users = cdb %>% 
  db_alldocs("_users", include_docs=TRUE, as = "json") %>% 
  fromJSON() %>% 
  .$rows %>% 
  .$doc %>% 
  filter(name != "root", !is.na(name)) %>% 
  .$name

# Read topics
topics = cdb %>% 
  db_alldocs("topics", include_docs=TRUE, as = "json") %>% 
  fromJSON() %>% 
  .$rows %>% 
  .$doc %>% 
  tbl_df() %>% 
  .$topic_id %>% 
  sort()

# Read user topics
user_topics = NULL
for (i in seq_along(users)) {
  user = users[[i]]
  if (user %in% db_list(cdb)) {
    
    usertop = tryCatch({
      cdb %>% 
        doc_get(user, 
                "data") %>% 
        .$topics %>% 
        unlist()
      }, 
        error = function(err) NULL
    )
    if (length(usertop)>0) {
      user_topics = user_topics %>% 
        bind_rows(data_frame(user = user, topic = usertop))
    }
  }
}

user_topics = user_topics %>% 
  mutate(point = 1) %>% 
  complete(user = users, topic = topics, fill = list(point=0))

# read paper ids
mlr_paper_ids = readLines(file.path(txt_path, "files.dat")) %>% 
  tools::file_path_sans_ext() %>% 
  basename()

# generate random user likes
if ("simu" %in% names(cfg)) {
  set.seed(cfg$simu$seed)
  n_likes = cfg$simu$n_likes
  
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

# save userids file
fn = file.path(data_path, 'userids.dat')
cat("writing", fn, "\n")

users %>% 
  writeLines(fn)

# save users file
fn = file.path(data_path, 'users.dat')
cat("writing", fn, "\n")

userlikes %>% 
  filter(!is.na(ctr_paper_id)) %>% 
  group_by(ctr_user_id) %>% 
  nest(ctr_paper_id, .key = "ctr_paper_ids") %>% 
  mutate(n = sapply(ctr_paper_ids, nrow),
         ctr_paper_ids = sapply(ctr_paper_ids, function(x) paste(x$ctr_paper_id, collapse=" "))) %>% 
  complete(ctr_user_id = seq(0, max(userlikes$ctr_user_id)), 
           fill=list(ctr_paper_ids = "", n=0)) %>% 
  transmute(out = paste(n, ctr_paper_ids)) %>% 
  .$out %>% 
  writeLines(fn)

# save items file
fn = file.path(data_path, 'items.dat')
cat("writing", fn, "\n")

userlikes %>% 
  group_by(ctr_paper_id) %>% 
  nest(ctr_user_id, .key = "ctr_user_ids") %>% 
  mutate(n = sapply(ctr_user_ids, nrow),
         ctr_user_ids = sapply(ctr_user_ids, function(x) paste(x$ctr_user_id, collapse=" "))) %>% 
  complete(ctr_paper_id = seq_along(mlr_paper_ids)-1, 
           fill=list(ctr_user_ids = "", n=0)) %>% 
  transmute(out = paste(n, ctr_user_ids)) %>% 
  .$out %>% 
  writeLines(fn)

# write user topics
#=====================

fn = file.path(data_path, 'theta_u.dat')
cat("writing", fn, "\n")

theta_u = user_topics %>% 
  spread(topic, point) %>%
  slice(match(users, user)) %>% 
  select(-user) %>% 
  tbl_df()

theta_u %>% 
  write_delim(fn, delim =" ", col_names = FALSE)
