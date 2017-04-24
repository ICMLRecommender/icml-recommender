#!/usr/bin/Rscript --slave

require(tidyr)
require(dplyr)
require(jsonlite)
require(sofa)
require(yaml)

args = commandArgs(TRUE)

cfg_file = "config.yml"
if (length(args>0))
  cfg_file = args[1]

cfg = yaml.load_file(cfg_file)

data_path = cfg$data$path

# read data
#====================

papers = file(file.path(data_path, "papers_topics.json")) %>% 
  fromJSON() %>% 
  tbl_df()

authors = file(file.path(data_path, "authors.json")) %>% 
  fromJSON() %>% 
  tbl_df()

sessions = file(file.path(data_path, "sessions.json")) %>% 
  fromJSON() %>% 
  tbl_df()

topics = file(file.path(data_path, "topics.json")) %>% 
  fromJSON() %>% 
  tbl_df()


# write to couchDB
#=============================
cdb = do.call(Cushion$new, cfg$couchdb)

# ping(cdb)
db_list(cdb)

cdb %>% db_create("items", delifexists=TRUE)
papers = papers %>% 
  mutate(`_id` = mlr_paper_id)
cdb %>% db_bulk_create("items", apply(papers, 1, function(x) toJSON(x, auto_unbox=TRUE)))

cdb %>% db_create("authors", delifexists=TRUE)
authors = authors %>% 
  mutate(`_id` = author_id)
cdb %>% db_bulk_create("authors", apply(authors, 1, function(x) toJSON(x, auto_unbox=TRUE)))


cdb %>% db_create("sessions", delifexists=TRUE)
sessions = sessions %>% 
  mutate(`_id` = session_id)
cdb %>% db_bulk_create("sessions", apply(sessions, 1, function(x) toJSON(x, auto_unbox=TRUE)))

cdb %>% db_create("topics", delifexists=TRUE)
topics = topics %>% 
  mutate(`_id` = topic_id)
cdb %>% db_bulk_create("topics", apply(topics, 1, function(x) toJSON(x, auto_unbox=TRUE)))
