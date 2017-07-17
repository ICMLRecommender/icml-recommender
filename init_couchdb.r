#!/usr/bin/Rscript --slave

require(tidyr)
require(dplyr)
require(jsonlite)
require(sofa)
require(yaml)
require(lazyeval)

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
  tbl_df()

authors = file(file.path(data_path, "authors.json")) %>% 
  fromJSON() %>% 
  tbl_df()

schedule = file(file.path(data_path, "schedule.json")) %>% 
  fromJSON() %>% 
  tbl_df()

sessions = file(file.path(data_path, "sessions.json")) %>% 
  fromJSON() %>% 
  tbl_df()

topics = file(file.path(data_path, "topics.json")) %>% 
  fromJSON() %>% 
  tbl_df()

invited_talks = file(file.path(data_path, "invited_talks.json")) %>% 
  fromJSON() %>% 
  tbl_df()

# write to couchDB
#=============================
cdb = do.call(Cushion$new, cfg$couchdb)

# ping(cdb)
db_list(cdb)

db_write = function(cdb, dbname, df, col_id) {
  if (dbname %in% db_list(cdb))
    cdb %>% db_delete(dbname=dbname)
  cdb %>% db_create(dbname)
  df = df %>%
    mutate_(.dots = list("_id" = interp(~as.list(gsub("[ \\+]" , "-", id)), id = as.name(col_id))))
  cdb %>% db_bulk_create(dbname, apply(df, 1, function(x) toJSON(x, auto_unbox=TRUE)))
}

cdb %>% db_write(paste0("items", suffix), papers, "paper_id")
cdb %>% db_write(paste0("authors", suffix), authors, "author_id")
cdb %>% db_write(paste0("schedule", suffix), schedule, "schedule_id")
cdb %>% db_write(paste0("sessions", suffix), sessions, "session_id")
cdb %>% db_write(paste0("topics", suffix), topics, "label")
cdb %>% db_write(paste0("invited_talks", suffix), invited_talks, "talk_id")
