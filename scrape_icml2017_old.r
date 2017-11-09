#!/usr/bin/Rscript --slave

library(tidyverse)
library(stringr)
library(jsonlite)
library(readxl)

readRenviron(".env")

data_path = file.path(Sys.getenv("DATA_ROOT"), Sys.getenv("DATA_DIR"))
raw_path = file.path(data_path, "raw")
abstracts_txt_path = file.path(data_path, Sys.getenv("ABSTRACTS_TXT_DIR"))

dir.create(raw_path, showWarnings = FALSE, recursive=TRUE)

# enable file downloads
dl_schedule = Sys.getenv("SCRAPE_DL_SCHEDULE") == "1"
write_txt = Sys.getenv("SCRAPE_WRITE_TXT") == "1"

# Papers and authors
#======================
fix_affiliation = function(affiliation, email) {
  ok = (affiliation == "CMU")
  affiliation[ok] = "Carnegie Mellon University"
  
  ok = (str_length(affiliation)==0)
  domains = email[ok] %>% 
    str_extract("\\w+\\.\\w+$")
  fix = list("cmu.edu" = "Carnegie Mellon University",
             "fb.com" = "Facebook AI Research",
             "syr.edu" = "Syracuse University",
             "gatech.edu" = "Georgia Tech",
             "google.com" = "Google Research",
             "lip6.fr" = "University of Paris 6 (UPMC)")
  affiliation[ok] = unlist(fix)[match(domains, names(fix))]
  return(affiliation)
}

author_duplicates = c("Alex Gittens", "Christopher Pal", "Frank Nielsen", "Ian Osband",
                      "Jiecao Chen")

paper_authors = read_xlsx(file.path(raw_path, Sys.getenv("PAPERS_XLSX_FILE"))) %>% 
  select(filename = "ID", title = "Title", abstract = "Abstract", authors = "Name (Org) <Email>") %>% 
  mutate(paper_id = as.integer(filename)) %>% 
  mutate(authors = strsplit(authors, ";")) %>% 
  mutate(authors = map(authors, ~set_names(as_tibble(str_match(gsub("*", "", .x, fixed = TRUE), 
                                                               "(.*?)\\s\\((.*?)\\)\\s<(.*?)>")[,2:4,drop=FALSE]), 
                                           c("author", "affiliation", "email")))) %>% 
  unnest() %>% 
  mutate(affiliation = fix_affiliation(affiliation, email)) %>% 
  group_by(author) %>% 
  arrange(affiliation) %>% 
  fill(affiliation) %>% 
  group_by(author, affiliation) %>%
  { mutate(ungroup(.), author_id = group_indices(.)) } %>%
  ungroup() %>% 
  group_by(author) %>% 
  mutate(author_id = if_else(author %in% author_duplicates,
                             first(author_id), author_id))

authors = paper_authors %>% 
  group_by(author_id, author) %>% 
  summarize(affiliations = list(unique(affiliation)),
            emails = list(unique(email)),
            paper_ids = list(unique(paper_id))) %>% 
  ungroup()

papers = paper_authors %>% 
  group_by(paper_id, title, abstract, filename) %>% 
  summarize(author_ids = list(unique(author_id))) %>% 
  ungroup()

# Write abstracts txt files
#===============================
if (write_txt) {
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
}

# Schedule (xl file)
#====================
schedule_file = file.path(raw_path, "schedule.xlsx")

if (dl_schedule)
  download.file(Sys.getenv("SCHEDULE_XLSX_URL"), schedule_file)

locations =  read_xlsx(schedule_file, n_max = 0) %>% 
  colnames()

locations = locations[grepl("^[^(X__)]", locations)][-1]

times = read_xlsx(schedule_file, range = "A3:D83",
                  col_names = c("date", "time_start", "time_end", "session_or_sub")) %>% 
  mutate(time_start = format(time_start, "%H:%M"),
         time_end = format(time_end, "%H:%M"))

cols = c(LETTERS, str_c("A", LETTERS))
schedule = list()
for (i in seq_along(locations)) {
  c1 = cols[5+(i-1)*3]
  c2 = cols[7+(i-1)*3]
  df = read_xlsx(schedule_file, range = str_c(c1, "3:", c2, "83"),
                 col_names = c("paper_id", "title", "poster_date"))
  schedule[[locations[i]]] = times %>% 
    bind_cols(df)
}

schedule = schedule %>% 
  bind_rows(.id = "location") %>% 
  drop_na(date) %>% 
  mutate(session_title = if_else(session_or_sub == "Session", title, NA_character_)) %>% 
  fill(session_title)

schedule = schedule %>% 
  filter(session_or_sub == "Session") %>% 
  select(-session_or_sub, -paper_id, -session_title, -poster_date) %>% 
  left_join(schedule %>% 
              filter(session_or_sub == "Sub") %>% 
              select(-session_or_sub) %>% 
              drop_na(paper_id) %>% 
              group_by(session_title) %>% 
              nest(time_start, time_end, paper_id, poster_date, .key = "talks"),
            by = c("title"="session_title")) %>% 
  mutate(schedule_id = seq_len(n())) %>% 
  mutate(type = if_else(map_lgl(talks, is.null), "Break", "Talk")) %>% 
  mutate(type = if_else(str_detect(title, "Keynote: "), "Keynote", type)) #%>% 
# mutate(keynote_speaker = str_match(title, "Keynote: (.*)")[,2])


papers_schedule = schedule %>% 
  filter(type == "Talk") %>% 
  select(talks, schedule_id) %>% 
  unnest()

papers = papers %>% 
  left_join(papers_schedule, by = "paper_id")

# Write json
#============
papers %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(data_path, "papers.json"))

authors %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(data_path, "authors.json"))

schedule %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(data_path, "schedule.json"))
