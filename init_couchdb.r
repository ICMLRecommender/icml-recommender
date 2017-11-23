#!/usr/bin/Rscript --slave

source("common.r")

# read data
#====================

if (file.exists(file.path(data_path, "papers_topics.json"))) {
  papers = file(file.path(data_path, "papers_topics.json")) %>%
    fromJSON() %>%
    as_tibble()
} else if (file.exists(file.path(data_path, "papers.json"))) {
  papers = file(file.path(data_path, "papers.json")) %>% 
    fromJSON() %>% 
    as_tibble()
}

if (file.exists(file.path(data_path, "authors.json"))) {
  authors = file(file.path(data_path, "authors.json")) %>% 
    fromJSON() %>% 
    as_tibble()
}

if (file.exists(file.path(data_path, "schedule.json"))) {
  schedule = file(file.path(data_path, "schedule.json")) %>% 
    fromJSON() %>% 
    as_tibble()
}

if (file.exists(file.path(data_path, "topics.json"))) {
  topics = file(file.path(data_path, "topics.json")) %>%
    fromJSON() %>%
    as_tibble()
}

if (file.exists(file.path(data_path, "topic_clusters.json"))) {
  topic_clusters = file(file.path(data_path, "topic_clusters.json")) %>%
    fromJSON() %>%
    as_tibble()
}

# write to couchDB
#=============================
cdb = do.call(Cushion$new, couchdb_args)

# ping(cdb)
# db_list(cdb)

if (exists("papers")) {
  cdb %>% db_write(str_c("papers", suffix), papers, paper_id, revs_limit=couchdb_revs_limit)
}
if (exists("authors")) {
  cdb %>% db_write(str_c("authors", suffix), authors, author_id, revs_limit=couchdb_revs_limit)
}
if (exists("schedule")) {
  cdb %>% db_write(str_c("schedule", suffix), schedule, session_id, revs_limit=couchdb_revs_limit)
}
if (exists("topics")) {
  cdb %>% db_write(str_c("topics", suffix), topics, topic_id, revs_limit=couchdb_revs_limit)
}
if (exists("topic_clusters")) {
  cdb %>% db_write(str_c("topic_clusters", suffix), topic_clusters, topic_cluster_id, revs_limit=couchdb_revs_limit)
}
