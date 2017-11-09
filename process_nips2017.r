#!/usr/bin/Rscript --slave

source("common.r")

# Read data 
# ====================
schedule = fromJSON(file.path(raw_path, "schedule.json")) %>% 
  as_tibble()

events = fromJSON(file.path(raw_path, "events.json")) %>% 
  as_tibble()

authors = fromJSON(file.path(raw_path, "authors.json")) %>% 
  as_tibble()

event_subjects = fromJSON(file.path(raw_path, "event_subjects.json")) %>% 
  as_tibble()

# Schedule 
# ====================
# fill missing session and session_id for invited talks, symposium and workshops
session_id_prefix = list("Invited Talk (Posner Lecture)" = "posn",
                         "Invited Talk" = "invt",
                         "Invited Talk (Breiman Lecture)" = "brei",
                         "Symposium" = "symp",
                         "Workshop" = "wksp")

schedule = schedule %>% 
  mutate(session = if_else(type %in% c("Workshop", "Invited Talk", "Symposium"), 
                           str_c(type, str_extract(day, boundary("word")), time_start, sep=" "),
                           session)) %>% 
  mutate(session = if_else(type %in% c("Invited Talk (Posner Lecture)", "Invited Talk (Breiman Lecture)"), 
                           type, session)) %>% 
  mutate(room = str_extract(location, "[^#]+") %>% 
           str_replace("Room", "") %>% 
           str_trim()) %>% 
  group_by(type) %>% 
  mutate(session_id = if_else(is.na(session_id), 
                              str_c(session_id_prefix[type], seq_len(n())), # fill missing session_ids with fake unique ids
                              session_id)) %>% 
  ungroup()

# add session_times 
locale = Sys.getlocale("LC_TIME")
Sys.setlocale("LC_TIME", "en_US.utf8")

schedule = schedule %>% 
  left_join(group_by(., session_id) %>% 
              summarize(session_time_start = first(time_start),
                        session_time_end = last(time_end)),
            by = "session_id") %>% 
  mutate(session_time = as.POSIXct(str_c(day, session_time_start), format = "%a %b %dth %I:%M %p"))

Sys.setlocale("LC_TIME", locale)

# Join tables
# ===================


# join schedule with events
schedule = schedule %>% 
  left_join(events, by = "event_id") %>% 
  mutate(authors = author_details) %>% 
  select(-author_details) %>% 
  mutate(authors = authors %>% map_if(map_lgl(authors, ~nrow(.)==0), ~NULL)) %>% 
  mutate(authors = authors %>% map_if(map_lgl(authors, negate(is.null)), ~select(., -author_url)))


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
if (exists("papers_proc")) {
  papers = schedule %>% 
    filter(type %in% c("Oral", "Spotlight", "Poster")) %>% 
    distinct(title, .keep_all = TRUE) %>% 
    select(title, authors, abstract, paper_url) %>%
    left_join(papers_proc %>%
                select(paper_url, filename, pdf_url, supp_pdf_url),
              by = "paper_url") %>%
    arrange(filename) %>%
    mutate(paper_id = seq_len(n()))
  
  schedule_papers = schedule %>% 
    filter(type %in% c("Oral", "Spotlight", "Poster")) %>% 
    select(type, session_id, event_id, paper_url, time_start, time_end, location) %>% 
    left_join(papers %>% 
                select(paper_id, paper_url), 
              by = "paper_url") %>% 
    select(-paper_url)
} else {
  papers = schedule %>% 
    filter(type %in% c("Oral", "Spotlight", "Poster")) %>% 
    distinct(title, .keep_all = TRUE) %>% 
    select(title, authors, abstract) %>%
    mutate(paper_id = seq_len(n())) %>% 
    mutate(filename = as.character(paper_id))
  
  schedule_papers = schedule %>% 
    filter(type %in% c("Oral", "Spotlight", "Poster")) %>% 
    select(type, session_id, event_id, title, time_start, time_end, location) %>% 
    left_join(papers %>% 
                select(paper_id, title), 
              by = "title") %>% 
    select(-title)
}

schedule = schedule %>% 
  left_join(schedule_papers, 
            by = c("type", "session_id", "event_id", "time_start", "time_end", "location"))

papers = papers %>%
  left_join(schedule_papers %>% 
              group_by(paper_id) %>% 
              nest(.key = "schedule"),
            by = "paper_id")

# join papers and schedule with event_subjects
paper_subjects = event_subjects %>% 
  left_join(schedule %>% 
              select(event_id, paper_id), 
            by="event_id") %>% 
  select(-event_id)

papers = papers %>%
  left_join(paper_subjects, by="paper_id")

schedule = schedule %>%
  left_join(paper_subjects, by="paper_id")
            
# join authors with papers
author_papers = papers %>%
  select(paper_id, authors) %>% 
  unnest() %>% 
  group_by(author_id, author) %>% 
  summarise(paper_ids = list(paper_id)) %>% 
  ungroup()

authors = authors %>%
  left_join(author_papers, by = c("author_id", "author"))

# simplification of schedule
papers = papers %>%
  mutate(spotlight_schedule = map(schedule, ~filter(., type == "Spotlight")),
         oral_schedule = map(schedule, ~filter(., type == "Oral")),
         poster_schedule = map(schedule, ~filter(., type == "Poster"))) %>% 
  select(-schedule)

# group schedule by session and nest
schedule = schedule %>% 
  group_by(session_id, session, type, day, room, session_time, session_time_start, session_time_end) %>% 
  nest(.key = "content") %>% 
  mutate(content = content %>% map_if(!(type %in% c("Invited Talk", "Invited Talk (Breiman Lecture)", "Invited Talk (Posner Lecture)", "Symposium")), 
                                      ~select(., -abstract))) %>% 
  mutate(content = content %>% map_if(!(type %in% c("Poster", "Demonstration")), 
                                      ~select(., -location))) %>% 
  mutate(content = content %>% map_if(!(type %in% c("Oral", "Spotlight", "Poster")), 
                                      ~select(., -paper_id, -subjects)))

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

