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

# read data
#====================

papers = file(file.path(data_path, "papers_topics.json")) %>% 
  fromJSON() %>% 
  as_tibble()

authors = file(file.path(data_path, "authors.json")) %>% 
  fromJSON() %>% 
  as_tibble()

schedule = file(file.path(data_path, "schedule.json")) %>% 
  fromJSON() %>% 
  as_tibble()

topics = file(file.path(data_path, "topics.json")) %>% 
  fromJSON() %>% 
  as_tibble()

topic_clusters = file(file.path(data_path, "topic_clusters.json")) %>% 
  fromJSON() %>% 
  as_tibble()

# write to couchDB
#=============================
cdb = do.call(Cushion$new, cfg$couchdb)

# ping(cdb)
# db_list(cdb)

db_write = function(cdb, dbname, data, id) {
  id = enquo(id)
  if (dbname %in% db_list(cdb))
    cdb %>% db_delete(dbname=dbname)
  cdb %>% db_create(dbname)
  if (!is.null(cfg$couchdb_revs_limit)) {
    httr::PUT(paste(cdb$make_url(), dbname, "_revs_limit", sep="/"), 
              cdb$get_headers(), body=as.character(cfg$couchdb_revs_limit))
  }
  if (!quo_name(id) %in% names(data)) {
    warning("missing ", quo_name(id), " in data")
    return(NULL)
  }
  docs = data %>%
    mutate(`_id` = str_replace_all(!!id, "[\\s\\+]" , "-")) %>% 
    split(.$`_id`) %>% 
    map_chr(~str_extract(toJSON(.x), "\\{.+\\}"))
  cdb %>% db_bulk_create(dbname, docs)
}

cdb %>% db_write(str_c("papers", suffix), papers, paper_id)
cdb %>% db_write(str_c("authors", suffix), authors, author_id)
cdb %>% db_write(str_c("schedule", suffix), schedule, session_id)
cdb %>% db_write(str_c("topics", suffix), topics, topic_id)
cdb %>% db_write(str_c("topic_clusters", suffix), topic_clusters, topic_cluster_id)
