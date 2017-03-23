require(jsonlite)
require(dplyr)
require(tidyr)
require(readr)
require(sofa)

# read data
#====================
data_path = "data/icml2016"

papers = file(file.path(data_path, "papers.json")) %>% 
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
cdb = Cushion$new()

# cdb = Cushion$new(host = "icml.papro.org.uk",
#                   path = "couchdb",
#                   port = NULL,
#                   transport = "https")

ping(cdb)
db_list(cdb)

if ("items" %in% db_list(cdb)) 
  cdb %>% db_delete("items")
cdb %>% db_create("items")
cdb %>% db_bulk_create("items", apply(papers, 1, function(x) toJSON(x, auto_unbox=TRUE)))

if ("authors" %in% db_list(cdb)) 
  cdb %>% db_delete("authors")
cdb %>% db_create("authors")
cdb %>% db_bulk_create("authors", apply(authors, 1, function(x) toJSON(x, auto_unbox=TRUE)))

if ("sessions" %in% db_list(cdb)) 
  cdb %>% db_delete("sessions")
cdb %>% db_create("sessions")
cdb %>% db_bulk_create("sessions", apply(sessions, 1, function(x) toJSON(x, auto_unbox=TRUE)))

if ("topics" %in% db_list(cdb)) 
  cdb %>% db_delete("topics")
cdb %>% db_create("topics")
cdb %>% db_bulk_create("topics", apply(topics, 1, function(x) toJSON(x, auto_unbox=TRUE)))
