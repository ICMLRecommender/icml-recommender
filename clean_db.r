
source("common.r")

cdb = do.call(Cushion$new, couchdb_args)

dbs = db_list(cdb)

admins = c("root", "adrien", "lazar", "valerio", "seb")

# clean admins
out = admins %>% 
  map(~safely(doc_delete)(cdb, ., "data"))

out = admins %>% 
  map(~safely(doc_delete)(cdb, ., "preferred_topics"))

out = admins %>% 
  map(~safely(doc_delete)(cdb, ., "recommendations"))

# clean other users
others = cdb %>% 
  db_alldocs("_users") %>% 
  .$rows %>% 
  map_chr("id") %>% 
  keep(str_detect, fixed("org.couchdb.user")) %>% 
  map_chr(str_replace, "org.couchdb.user:", "") %>% 
  discard(~. %in% admins)

out = others %>% 
  map(~doc_delete(cdb, "_users", paste0("org.couchdb.user:", .)))

out = others %>% 
  map(~safely(db_delete)(cdb, .))


# clean userlikes
out = cdb %>% 
  db_alldocs("userlikes") %>%  
  .$rows %>% 
  map_chr("id") %>% 
  map(~doc_delete(cdb, "userlikes", .))

# clean userlikes_reversed
out = cdb %>% 
  db_alldocs("userlikes_reversed") %>%  
  .$rows %>% 
  map_chr("id") %>% 
  map(~doc_delete(cdb, "userlikes_reversed", .))

# clean comments
out = cdb %>% 
  db_alldocs("usercomments") %>%  
  .$rows %>% 
  map_chr("id") %>% 
  map(~doc_delete(cdb, "usercomments", .))

# clean password_reset
out = cdb %>% 
  db_alldocs("password_reset") %>%  
  .$rows %>% 
  map_chr("id") %>% 
  map(~doc_delete(cdb, "password_reset", .))
