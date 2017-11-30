#!/usr/bin/Rscript --slave

source("common.r")

# Read data 
# ====================
# schedule_old = fromJSON(file.path(raw_path, "schedule_old.json")) %>% 
#   as_tibble()
# 
# events_old = fromJSON(file.path(raw_path, "events_old.json")) %>% 
#   as_tibble()
# 
# authors_old = fromJSON(file.path(raw_path, "authors_old.json")) %>% 
#   as_tibble()

schedule = fromJSON(file.path(raw_path, "schedule.json")) %>% 
  as_tibble()

events = fromJSON(file.path(raw_path, "events.json")) %>% 
  as_tibble()

authors = fromJSON(file.path(raw_path, "authors.json")) %>% 
  as_tibble()

# event_subjects = fromJSON(file.path(raw_path, "event_subjects.json")) %>% 
#   as_tibble()

papers_cmt = read_tsv(file.path(raw_path, "Copy2 of NIPS2017 Accepted Papers - NIPS2017.tsv"), 
                  col_types = "cc__c_",
                  col_names = c("paper_id",
                              "title",
                              "subject_areas"),
                  skip=1)

# fix titles
#=================

# some papers_cmt titles have been changed
titles_new = read_delim(file.path(raw_path, "titles_new.csv"), delim=";", col_types = "cc")

papers_cmt = papers_cmt %>% 
  left_join(titles_new, by = "title") %>% 
  mutate(title = ifelse(!is.na(title_new), title_new, title)) %>% 
  select(-title_new)

# some oral or spotlights have title different than the poster title
titles_poster = read_delim(file.path(raw_path, "titles_poster.csv"), delim=";", col_types = "cc")

schedule = schedule %>% 
  left_join(titles_poster, by="title") %>% 
  mutate(title = ifelse(type %in% c("Oral", "Spotlight") & !is.na(title_poster), title_poster, title)) %>% 
  select(-title_poster)
  

# Session labels
#====================

session_labels_file = file.path(raw_path, "session_labels.csv")
download.file(session_labels_url, session_labels_file)

session_labels = read_delim(session_labels_file, delim=",", na="", trim_ws = TRUE, col_types = "cc")

# Schedule 
# ====================

# fill missing urls

schedule = schedule %>% 
  arrange(desc(type == "Poster")) %>%
  group_by(title) %>%
  fill(paper_url, poster_url, code_url, video_url) %>% 
  ungroup()

# fill missing session and session_id for breaks, invited talks, symposium and workshops
session_id_prefix = list("Break" ="brk",
                         "Invited Talk (Posner Lecture)" = "posn",
                         "Invited Talk" = "invt",
                         "Invited Talk (Breiman Lecture)" = "brei",
                         "Symposium" = "symp",
                         "Workshop" = "wksp",
                         "Oral" = "oral",
                         "Tutorial" = "tuto",
                         "Talk" = "talk")


get_session_labels = function(session) {
  session_topics = str_split(session, ", +")[[1]]
  ind = match(session_topics, session_labels$session_topic)
  labels = session_labels$session_label[ind]
  return(labels)
}


schedule = schedule %>% 
  mutate(session_location = str_extract(location, "[^#]+") %>% 
           str_replace("Room-?", "") %>% 
           str_trim()) %>% 
  mutate(session_labels = if_else(type %in% c("Oral", "Spotlight"), session %>% map(get_session_labels), NULL)) %>% 
  mutate(session_id = if_else(type %in% c("Oral", "Tutorial"), NA_character_, session_id)) %>% 
  mutate(session = if_else(is.na(session_id), title, session)) %>% 
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

# Subject areas labels
#=======================
papers_cmt = papers_cmt %>% 
  mutate(subject_areas = str_split(subject_areas, "; ") %>% 
           map(str_replace_all, "\\*", "") %>% 
           map(str_replace_all, " +", " ") %>% 
           map(str_trim) %>% 
           map(str_split_fixed, fixed("\\"), 2) %>% 
           map(as_tibble) %>% 
           map(set_colnames, str_c("subject_area_", 1:2)))
  # mutate(authors = str_split(authors, "; ") %>% 
  #          map(str_replace_all, " +", " ") %>% 
  #          map(str_trim) %>% 
  #          map(str_split_fixed, ", \\*", 2) %>% 
  #          map(as_tibble) %>% 
  #          map(set_colnames, c("author", "affiliation")))
  
subject_labels_file = file.path(raw_path, "subject_labels.csv")
download.file(subject_labels_url, subject_labels_file)

subject_labels = read_delim(subject_labels_file, delim=",", na="", trim_ws = TRUE, col_types = 'ccc') %>% 
  mutate(subject_area_2 = if_else(is.na(subject_area_2), "", subject_area_2), # replace na with empty string "" for the join
         labels = str_split(labels, " +"))

papers_subject_areas = papers_cmt %>% 
  mutate(subject_areas = subject_areas %>% map(slice, 1)) %>% 
  unnest() %>% 
  left_join(subject_labels, by = c("subject_area_1", "subject_area_2")) %>% 
  rename(subject_labels = labels)

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


# join schedule with papers
papers = schedule %>% 
  filter(type %in% c("Oral", "Spotlight", "Poster")) %>% 
  select(title, abstract, authors, paper_url, poster_url, code_url, video_url) %>%
  distinct(title, .keep_all = TRUE) %>% 
  left_join(papers_subject_areas, by = "title") %>% 
  mutate(filename = str_c("paper_", paper_id))

schedule_papers = schedule %>% 
  filter(type %in% c("Oral", "Spotlight", "Poster")) %>% 
  select(type, session_id, event_id, title, time_start, time_end, location) %>% 
  left_join(papers %>% 
              select(paper_id, title, subject_labels), by = "title") %>% 
  select(-title)

schedule = schedule %>% 
  left_join(schedule_papers, 
            by = c("type", "session_id", "event_id", "time_start", "time_end", "location"))

papers = papers %>%
  left_join(schedule_papers %>% 
              group_by(paper_id) %>% 
              nest(.key = "schedule"),
            by = "paper_id")

# # join papers and schedule with event_subjects
# paper_subjects = event_subjects %>% 
#   left_join(schedule %>% 
#               select(event_id, paper_id), 
#             by="event_id") %>% 
#   select(-event_id)
# 
# papers = papers %>%
#   left_join(paper_subjects, by="paper_id")
# 
# schedule = schedule %>%
#   left_join(paper_subjects, by="paper_id")
            
# join authors with papers
author_papers = papers %>%
  select(paper_id, authors) %>% 
  unnest() %>% 
  group_by(author_id, author) %>% 
  summarise(paper_ids = list(paper_id)) %>% 
  ungroup()

authors = authors %>%
  left_join(author_papers, by = c("author_id", "author")) %>% 
  mutate(bio = if_else(bio=="Bio is unavailable.", NA_character_, bio))

# simplification of schedule
papers = papers %>%
  mutate(spotlight_schedule = map(schedule, ~filter(., type == "Spotlight")),
         oral_schedule = map(schedule, ~filter(., type == "Oral")),
         poster_schedule = map(schedule, ~filter(., type == "Poster"))) %>% 
  select(-schedule)

# save session_labels because it cannot be grouped by
schedule_session_labels = schedule %>% 
  filter(map_lgl(session_labels, negate(is.null))) %>%
  mutate(session_labels = if_else(type %in% "Oral", subject_labels, session_labels)) %>%
  select(session_id, session_labels) %>% 
  distinct(session_id, .keep_all = TRUE)

# nest schedule by session
schedule_nested = schedule %>% 
  select(-session_labels) %>% 
  group_by(session_id, session, type, day, session_location, session_time, session_time_start, session_time_end) %>%
  nest(.key = "content") %>% 
  mutate(content = content %>% 
           map_if(type %in% c("Break", "Poster", "Spotlight"), 
                  ~select(., -abstract)) %>% 
           map_if(type %in% c("Break"), 
                  ~select(., -authors)) %>% 
           map_if(!(type %in% c("Workshop")), 
                  ~select(., -event_schedule)) %>% 
           map_if(!(type %in% c("Poster", "Demonstration")), 
                  ~select(., -location)) %>% 
           map_if(!(type %in% c("Oral", "Spotlight", "Poster")), 
                  ~select(., -paper_id, -subject_labels, -paper_url, -poster_url, -code_url, -video_url))) %>% 
  left_join(schedule_session_labels, by="session_id") # join with schedule_session_labels 

# Write json
#============
papers %>% 
  toJSON(pretty=TRUE) %>%
  write(file.path(data_path, "papers.json"))

authors %>%
  toJSON(pretty=TRUE) %>%
  write(file.path(data_path, "authors.json"))

schedule_nested %>% 
  toJSON(pretty=TRUE) %>%
  write(file.path(data_path, "schedule.json"))

