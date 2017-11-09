#!/usr/bin/Rscript --slave

source("common.r")

papers = fromJSON(file.path(data_path, "papers.json")) %>% 
  as_tibble()

# Write abstracts txt files
#===============================
dir.create(abstracts_txt_path, showWarnings = FALSE, recursive = TRUE)
papers %>% 
  split(.$paper_id) %>% 
  walk(function(x){
    fn = file.path(abstracts_txt_path, str_c(x$filename, ".txt"))
    fc = file(fn)
    writeLines(x$abstract, fc)
    close(fc)
    fn
  })
