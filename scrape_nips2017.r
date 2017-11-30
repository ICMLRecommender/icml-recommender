#!/usr/bin/Rscript --slave

source("common.r")

dir.create(raw_path, showWarnings = FALSE, recursive=TRUE)

# scrape from website or other files

schedule_url = "https://nips.cc/Conferences/2017/Schedule"

# Schedule (website)
# ====================

html = read_html(schedule_url) %>% 
  html_nodes("div.col-xs-12.col-sm-9 > div[onclick] > div[id]")

schedule = html %>% 
  map_df(parse_schedule, .pb = progress_estimated(length(html)))

schedule %>% 
  toJSON(pretty = TRUE) %>% 
  write(file.path(raw_path, "schedule.json"))


# Events (website)
#==============
events = schedule$event_id %>% 
{ map_df(., parse_event, 
         schedule_url = schedule_url,
         .pb = progress_estimated(length(.))) }

events %>% 
  toJSON(pretty = TRUE) %>% 
  write(file.path(raw_path, "events.json"))

# Authors (website)
#==============
authors = events %>% 
  select(author_details) %>% 
  unnest() %>% 
  group_by(author_id) %>% 
  summarize(author_url = first(author_url)) %>% 
  mutate(details = author_url %>% map(safely(parse_author), .pb = progress_estimated(n())))

# run again for failures (timeout)
authors_fail = authors %>% 
  filter(details %>% map("error") %>% map_lgl(negate(is.null))) %>% 
  mutate(details = author_url %>% map(safely(parse_author), .pb = progress_estimated(n())))

authors = authors %>% 
  bind_rows(authors_fail) %>% 
  mutate(result = details %>% map("result") %>% map(as_tibble)) %>% 
  select(-details) %>% 
  unnest(result)

authors %>% 
  toJSON(pretty = TRUE) %>% 
  write(file.path(raw_path, "authors.json"))

# # Subjects (website)
# #==========================
# 
# html = str_c(schedule_url, "?bySubject") %>% 
#   read_html() %>% 
#   html_nodes("#bySubjectForm > label[for]")
# 
# subject_ids = html %>% 
#   html_attr("for") %>% 
#   str_extract("\\d+")
# 
# subject_labels = html %>% 
#   html_text() %>% 
#   str_trim()
# 
# 
# subjects = data_frame(subject_id = subject_ids,
#                       subject = subject_labels) %>% 
#   mutate(event_ids = subject_id %>% map(parse_subject, 
#                                         schedule_url = schedule_url, 
#                                         .pb = progress_estimated(n())))
#   
# event_subjects = subjects %>% 
#   unnest() %>% 
#   group_by(event_id) %>% 
#   nest(.key = "subjects")
# 
# event_subjects %>% 
#   toJSON(pretty = TRUE) %>% 
#   write(file.path(raw_path, "event_subjects.json"))
