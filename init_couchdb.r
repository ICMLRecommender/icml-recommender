#!/usr/bin/Rscript --slave

source("common.r")

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
cdb = do.call(Cushion$new, couchdb_args)

# ping(cdb)
# db_list(cdb)

cdb %>% db_write(str_c("papers", suffix), papers, paper_id, revs_limit=couch_revs_limit)
cdb %>% db_write(str_c("authors", suffix), authors, author_id, revs_limit=couch_revs_limit)
cdb %>% db_write(str_c("schedule", suffix), schedule, session_id, revs_limit=couch_revs_limit)
cdb %>% db_write(str_c("topics", suffix), topics, topic_id, revs_limit=couch_revs_limit)
cdb %>% db_write(str_c("topic_clusters", suffix), topic_clusters, topic_cluster_id, revs_limit=couch_revs_limit)
