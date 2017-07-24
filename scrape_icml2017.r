#!/usr/bin/Rscript --slave

library(rvest)
library(tidyverse)
library(jsonlite)
library(yaml)
library(stringr)
library(readxl)

args = commandArgs(TRUE)

cfg_file = "config.yml"
if (length(args>0))
  cfg_file = args[1]

cfg = yaml.load_file(cfg_file)

data_path = cfg$data$path
txt_path = cfg$data$txt_path

dir.create(data_path, showWarnings = FALSE)

schedule_url = "https://2017.icml.cc/Conferences/2017/Schedule"
xl_schedule_url = "https://docs.google.com/spreadsheets/d/15jcUEnFTwtpvzgS4Hxl2EePNT6tZsnOq8JP3pMGwdYg/export?format=xlsx&id=15jcUEnFTwtpvzgS4Hxl2EePNT6tZsnOq8JP3pMGwdYg"
keynotes_url = "https://2017.icml.cc/Conferences/2017/InvitedSpeakers"

# enable file downloads
dl_schedule = cfg$scrape$dl_schedule
write_txt = cfg$scrape$write_txt

# Papers and authors
#======================
papers_file = file.path(data_path, "accepted papers -- Camera_ReadyPapers_test_removed.xlsx")

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

paper_authors = read_xlsx(papers_file) %>% 
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
  dir.create(txt_path, showWarnings = FALSE)
  papers %>% 
    split(.$paper_id) %>% 
    walk(function(x){
      fn = file.path(txt_path, str_c(x$filename, ".txt"))
      fc = file(fn)
      writeLines(x$abstract, fc)
      close(fc)
      fn
    })
}

# Schedule
#==============
schedule_file = file.path(data_path, "schedule.xlsx")

if (dl_schedule)
  download.file(xl_schedule_url, schedule_file)

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
  mutate(type = if_else(str_detect(title, "Keynote: "), "Keynote", type)) %>% 
  mutate(keynote_speaker = str_match(title, "Keynote: (.*)")[,2])

# Invited talks
#===============================
html = read_html(keynotes_url) %>% 
  html_nodes("div.col-xs-12:nth-child(5) > div:nth-child(1) > div:nth-child(1)") %>% 
  html_children()

i_talk = 0
keynotes = NULL
for (i in seq_along(html)) {
  nm = html[[i]] %>% html_name()
  txt = html[[i]] %>% html_text() %>% str_trim()
  if (nm == "h3") {
    if (i_talk>0) {
      keynotes = keynotes %>% 
        bind_rows(as_tibble(talk))
    }
    i_talk = i_talk+1
    talk = list(title = txt)
  }
  if (nm == "h4") {
    m = str_match(txt, "(.*?)\\s\\((.*?)\\)")
    talk$speaker = m[2]
    talk$affiliation = m[3]
  }
  if (nm == "p" && str_length(txt)>0) {
    if (str_detect(txt, "^Abstract:"))
      talk$abstract = txt
    if (is.null(talk$bio))
      talk$bio = txt
    else
      talk$bio = talk$bio %>% str_c(txt, sep = "\n")
  }
}

keynotes = keynotes %>% 
  bind_rows(as_tibble(talk)) %>% 
  mutate(keynote_speaker = speaker) %>% 
  group_by(keynote_speaker) %>% 
  nest(.key = "keynote")

schedule = schedule %>% 
  left_join(keynotes, by = "keynote_speaker") %>% 
  mutate(talks = if_else(type == "Keynote", keynote, talks)) %>% 
  select(-keynote_speaker, -keynote)

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

