#!/usr/bin/Rscript --slave

library(tidyverse, quietly=TRUE)
library(stringr, quietly=TRUE)
library(jsonlite, quietly=TRUE)

readRenviron(".env")

data_path = file.path(Sys.getenv("DATA_ROOT"), Sys.getenv("DATA_DIR"))
raw_path = file.path(data_path, "raw")

# Read data 
#==============================
papers_proc = fromJSON(file.path(raw_path, "papers_proc.json")) %>% 
  as_tibble()

schedule = fromJSON(file.path(raw_path, "schedule.json")) %>% 
  as_tibble()

events = fromJSON(file.path(raw_path, "events.json")) %>% 
  as_tibble()

authors = fromJSON(file.path(raw_path, "authors.json")) %>% 
  as_tibble()

# Session labels
#==================
session_labels_file = file.path(raw_path, "session_labels.csv")

download.file(Sys.getenv("SESSION_LABELS_CSV_URL"), session_labels_file)

session_labels = read_delim(session_labels_file, delim = ",", trim_ws = TRUE) %>% 
  mutate(session_labels = str_split(session_labels, "\\s+"))

# # Paper authors from CMT file
# #===========================
# parse_row = function(x) {
#   out = list(id = x[[1]]$Data[[1]],
#              title = x[[2]]$Data[[1]],
#              abstract = x[[3]]$Data[[1]],
#              authors = x[[6]]$Data[[1]] %>% 
#                str_replace_all("\\*", "") %>% 
#                str_split(">") %>% 
#                .[[1]] %>% 
#                str_match("(.*?)\\s+\\((.*?)\\)\\s+<(.*+)") %>% 
#                {.[,2:4,drop=FALSE]} %>% 
#                as_tibble() %>% 
#                set_names(c("author", "affiliation", "email")) %>% 
#                drop_na(author) %>% 
#                list())
# }
# 
# paper_authors = xml2::read_xml(file.path(raw_path, "Camera Ready Papers.xls")) %>% 
# { xml2::xml_children(.)[[2]] } %>% 
#   xml2::as_list() %>% 
#   {.$Table[-(1:2)]} %>% 
#   map(parse_row) %>% 
#   bind_rows()
# 
# fix_affiliation = function(affiliation, email) {
#   ok = (affiliation == "CMU")
#   affiliation[ok] = "Carnegie Mellon University"
#   
#   ok = (str_length(affiliation)==0)
#   domains = email[ok] %>% 
#     str_extract("\\w+\\.\\w+$")
#   fix = list("cmu.edu" = "Carnegie Mellon University",
#              "fb.com" = "Facebook AI Research",
#              "syr.edu" = "Syracuse University",
#              "gatech.edu" = "Georgia Tech",
#              "google.com" = "Google Research",
#              "lip6.fr" = "University of Paris 6 (UPMC)")
#   affiliation[ok] = unlist(fix)[match(domains, names(fix))]
#   return(affiliation)
# }
# 
# author_duplicates = c("Alex Gittens", "Christopher Pal", "Frank Nielsen", "Ian Osband",
#                       "Jiecao Chen")
# 
# paper_authors = paper_authors %>% 
#   unnest() %>% 
#   mutate(affiliation = fix_affiliation(affiliation, email)) %>%
#   group_by(author) %>% 
#   arrange(affiliation) %>% 
#   fill(affiliation) %>% 
#   group_by(author, affiliation) %>%
#   { mutate(ungroup(.), author_id = group_indices(.)) } %>%
#   ungroup() %>% 
#   group_by(author) %>% 
#   mutate(author_id = if_else(author %in% author_duplicates,
#                              first(author_id), author_id)) %>% 
#   ungroup()
# 
# authors = paper_authors %>% 
#   group_by(author_id, author) %>% 
#   summarize(affiliations = list(unique(affiliation)),
#             emails = list(unique(email)),
#             paper_ids = list(unique(paper_id))) %>% 
#   ungroup()
# 
# # Join authors
# shorten_title = function(title) {
#   
#   fix = list("Differentially Private Learning of Graphical Models using CGMs" = "Differentially Private Learning of Undirected Graphical Models Using Collective Graphical Models",
#              "Visualizing and Understanding Multilayer Perceptron Models: A Case Study in Speech Processing" = "Understanding the Representation and Computation of Multilayer Perceptrons: A Case Study in Speech Recognition")
#   
#   for (i in seq_along(fix_title)) {
#     title[title == names(fix_title)[i]] = fix[[i]]
#   }
#   
#   title %>% 
#     str_trim() %>% 
#     str_to_lower() %>% 
#     tm::removeWords(c(tm::stopwords("en"),
#                       "using")) %>% 
#     str_replace("optmization", "optimization") %>% 
#     str_replace("regresssion", "regression") %>% 
#     str_replace("processes", "process") %>% 
#     str_replace("mcmc", "markov chain monte carlo") %>% 
#     str_replace("rnns", "recurrent networks") %>% 
#     iconv(to='ASCII//TRANSLIT') %>%
#     str_replace_all("[^\\w]+", "")
# }
# 
# papers = papers %>%
#   select(paper_id, title) %>%
#   mutate(title_short = shorten_title(title)) %>%
#   left_join(paper_authors %>%
#               select(title, author_id) %>%
#               mutate(title_short = shorten_title(title)) %>%
#               select(-title) %>%
#               nest(author_id, .key = "author_ids"),
#             by = "title_short") %>% 
#   mutate(author_ids = map(author_ids, "author_id"))

# Schedule 
# ====================

# fill missing session and session_id 
schedule = schedule %>% 
  mutate(session = if_else(is.na(session), title, session)) %>% 
  mutate(session_id = if_else(is.na(session_id), str_c("b", seq_len(n())), session_id)) %>% 
  mutate(type = if_else(str_detect(session, "Award"), "Award", type)) # add Award type

# add session_times 
locale = Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "en_US.utf8")

schedule = schedule %>% 
  mutate(session_time = as.POSIXct(str_c(day, session_time_start), format = "%a %b %dth %I:%M %p")) %>% 
  left_join(group_by(., session_id) %>% 
              summarize(session_time_start = first(time_start),
                        session_time_end = last(time_end)),
            by = "session_id") %>% 
  mutate(session_time = as.POSIXct(str_c(day, session_time_start), format = "%a %b %dth %I:%M %p"))

Sys.setlocale("LC_TIME", locale)

# fix paper_url
paper_urls = schedule %>% 
  filter(type %in% c("Talk", "Poster")) %>% 
  select(type, event_id, title, paper_url) %>% 
  group_by(title) %>% 
  arrange(is.na(paper_url)) %>% 
  fill(paper_url) %>% 
  ungroup()

schedule = schedule %>% 
  select(-paper_url) %>% 
  left_join(paper_urls, by = c("event_id", "type", "title"))

# Join tables
# ===================

# join schedule with events
schedule = schedule %>% 
  left_join(events, by = "event_id") %>% 
  mutate(authors = author_details) %>% 
  select(-author_details) %>% 
  mutate(authors = authors %>% map_if(map_lgl(authors, ~nrow(.x)==0), ~NULL)) %>% 
  mutate(authors = authors %>% map_if(map_lgl(authors, negate(is.null)), ~select(.x, -author_url)))

# join schedule with session_labels
schedule = schedule %>% 
  mutate(session_topic = session %>% str_replace("\\d", "") %>% str_trim()) %>% 
  left_join(session_labels, by = "session_topic")

# join schedule with authors
schedule_authors = schedule %>% 
  select(authors, type, session_id, event_id) %>% 
  filter(map_lgl(authors, negate(is.null))) %>% 
  unnest() %>% 
  group_by(author_id, author) %>% 
  nest(.key = "schedule")

authors = authors %>% 
  left_join(schedule_authors, by = c("author_id", "author"))

# join schedule with papers_proc
papers = schedule %>% 
  filter(type %in% c("Talk", "Poster")) %>% 
  distinct(paper_url, .keep_all = TRUE) %>% 
  select(title, authors, abstract, paper_url) %>% 
  left_join(papers_proc %>% 
              select(paper_url, filename, pdf_url, supp_pdf_url), 
            by = "paper_url") %>% 
  arrange(filename) %>% 
  mutate(paper_id = seq_len(n()))

schedule_papers = schedule %>% 
  filter(type %in% c("Talk", "Poster")) %>% 
  select(type, session_id, event_id, paper_url, time_start, time_end, poster_location) %>% 
  left_join(papers %>% 
              select(paper_id, paper_url), 
            by = "paper_url") %>% 
  select(-paper_url)

schedule = schedule %>% 
  left_join(schedule_papers, 
            by = c("type", "session_id", "event_id", "time_start", "time_end", "poster_location")) 

papers = papers %>%
  left_join(schedule_papers %>% 
              group_by(paper_id) %>% 
              nest(.key = "schedule"),
            by = "paper_id")

paper_labels = schedule %>% 
  select(paper_id, session_topic, session_labels) %>% 
  filter(map_lgl(session_labels, negate(is.null))) %>% 
  distinct(paper_id, .keep_all = TRUE)

papers = papers %>%
  left_join(paper_labels, by = "paper_id")

paper_authors = schedule %>% 
  select(paper_id, authors) %>% 
  drop_na(paper_id) %>% 
  distinct(paper_id, .keep_all = TRUE)

papers = papers %>%
  select(-authors) %>% 
  left_join(paper_authors, by = "paper_id")

# join authors with papers
author_papers = paper_authors %>% 
  unnest() %>% 
  group_by(author_id, author) %>% 
  summarise(paper_ids = list(paper_id)) %>% 
  ungroup()

authors = authors %>%
  left_join(author_papers, by = c("author_id", "author"))

# simplification for Lazar
papers = papers %>%
  mutate(talk_schedule = map(schedule, ~filter(.x, type == "Talk") %>% select(-poster_location)),
         poster_schedule = map(schedule, ~filter(.x, type == "Poster"))) %>% 
  select(-schedule)

# group schedule by session and nest
schedule = schedule %>% 
  select(-paper_url) %>% 
  mutate(type = if_else(str_detect(session, "Award"), "Award", type)) %>% 
  group_by(session_id, session, type, day, location, session_time, session_time_start, session_time_end) %>% 
  nest(.key = "content") %>% 
  mutate(content = content %>% map_if(type %in% c("Talk", "Poster"), ~select(.x, -abstract))) %>% 
  mutate(content = content %>% map_if(type != "Poster", ~select(.x, -poster_location))) %>% 
  mutate(content = content %>% map_if(!(type %in% c("Talk", "Poster")), ~select(.x, -paper_id))) %>% 
  mutate(content = content %>% map_if(type %in% c("Break", "Award"), ~NULL))

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
