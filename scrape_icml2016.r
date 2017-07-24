#!/usr/bin/Rscript --slave

library(rvest)
library(tidyverse)
library(jsonlite)
library(yaml)
library(stringr)

args = commandArgs(TRUE)

cfg_file = "config.yml"
if (length(args>0))
  cfg_file = args[1]

cfg = yaml.load_file(cfg_file)

data_path = cfg$data$path

dir.create(data_path, showWarnings = FALSE)

papers_url = "http://proceedings.mlr.press/v48/"
schedule_url = "http://icml.cc/2016/?page_id=1839"
reviews_url = "http://icml.cc/2016/reviews/"
rebuttals_url = "http://icml.cc/2016/rebuttals/"
invited_talks_url = "http://icml.cc/2016/?page_id=93"
root_url = "http://icml.cc"
conf_url = "http://icml.cc/2016/"

# enable file downloads
dl_pdf = cfg$scrape$dl_pdf
dl_supp_pdf = cfg$scrape$dl_supp_pdf
dl_review = cfg$scrape$dl_review
dl_rebuttal = cfg$scrape$dl_rebuttal

if (dl_pdf || dl_supp_pdf)
  dir.create(file.path(data_path, "papers"), showWarnings = FALSE)

if (dl_review)
  dir.create(file.path(data_path, "reviews"), showWarnings = FALSE)

if (dl_rebuttal)
  dir.create(file.path(data_path, "rebuttals"), showWarnings = FALSE)

# Papers
#======================
parse_paper = function(url) {
  html = read_html(url)
  
  out = list(url = url)
  
  out$filename = out$url %>% 
    tools::file_path_sans_ext() %>% 
    basename()
  
  out$title = html %>% 
    html_nodes("h1") %>% 
    html_text() %>% 
    # str_replace_all("\\\\[()]", "$") %>% 
    str_trim()
  
  out$authors = html %>% 
    html_nodes("#authors") %>% 
    html_text() %>% 
    str_trim() %>% 
    str_split("[,\n\t]+")
  
  out$abstract = html %>% 
    html_nodes("#abstract") %>% 
    html_text() %>% 
    # str_replace_all("\\\\[()]", "$") %>% 
    str_trim()
  
  out$pdf_url = html %>% 
    html_nodes("#extras ul li:nth-child(1) a") %>% 
    html_attr("href")
  
  out$supp_pdf_url = html %>% 
    html_nodes("#extras ul li:nth-child(2) a") %>% 
    html_attr("href")
  
  if (length(out$supp_pdf_url)==0)
    out$supp_pdf_url = NA
  
  fn = file.path(data_path, "papers", basename(out$pdf_url))
  if (dl_pdf && !file.exists(fn))
    download.file(out$pdf_url, fn)
  
  if (!is.na(out$supp_pdf_url) && nchar(out$supp_pdf_url)>0) {
    fn = file.path(data_path, "papers", basename(out$supp_pdf_url))
    if (dl_supp_pdf && !file.exists(fn))
      download.file(out$supp_pdf_url, fn)
  }
  
  return(as_tibble(out))
}

html = read_html(papers_url)

urls = html %>% 
  html_nodes("div.paper p.links a") %>% 
  html_attr("href")

papers = data_frame(url = urls) %>% 
  filter(grepl("^.*\\.html$", url)) %>% 
  group_by(url) %>% 
  do(parse_paper(.$url)) %>% 
  ungroup()

# Invited speakers
#===============================
html = read_html(invited_talks_url) %>% 
  html_nodes("#main #content")

ids = html %>% 
  html_nodes("h2") %>% 
  html_attr("id")

invited_talks = NULL
for (i in seq_along(ids)) {
  talk = list(talk_id = ids[i])
  
  txt = html %>% 
    html_node(str_c("#", ids[i])) %>% 
    html_text() %>% 
    str_split("–") %>% .[[1]] %>% 
    str_trim()
  talk$speaker = txt[1]
  talk$title = txt[2]
  
  talk$url = str_c(invited_talks_url, "#", ids[i])
  
  talk$affiliation = html %>% 
    html_node(str_c("#", ids[i], " + h3")) %>% 
    html_text()
  
  talk$portrait_url = html %>% 
    html_node(str_c("#", ids[i], " ~ .medaillon .circle")) %>% 
    html_attr("src")
  
  talk$bio = html %>% 
    html_node(str_c("#", ids[i], " ~ p")) %>% 
    html_text()
  
  talk$abstract = html %>% 
    html_node(str_c("#", ids[i], " ~ p em")) %>% 
    html_text()
  
  talk$slides_url = html %>% 
    html_node(str_c("#", ids[i], " ~ p em span.slides a")) %>% 
    html_attr("href") %>% 
    str_c(root_url, .)
  
  invited_talks = invited_talks %>% 
    bind_rows(as_tibble(talk))
}

# Schedule Table
#===============================

html = read_html(schedule_url) %>% 
  html_nodes("#schedule > table[border] > tbody > tr")

schedule = NULL
for (i in seq_along(html)) {
  cl = html[i] %>% html_attr("class") %>% 
    str_split(" ") %>% 
    .[[1]]
  
  sch = list()
  
  if (any(cl == "sep")) {
    txt = html[i] %>% 
      html_children() %>% 
      html_text() %>% 
      str_trim()
    day = txt[1]
    rooms = txt[-1]
    next()
  } else {
    cells = html[i] %>% 
      html_children()
    time = cells[1] %>% 
      html_text() %>% 
      str_split(" – ") %>% 
      .[[1]]
    cells = cells[-1]
    
    if (length(time) == 0)
      time = c(last_time_end, "02:00") # fix for Lunch
    
    sch$day = day
    sch$time_start = time[1]
    sch$time_end = time[2]
    sch$title = cells %>% html_text()
    sch$location = NA
    sch$type = NA
    sch$talk_id = NA
    sch$session_id = NA
    if (any(cl %in% c("allcols", "allcolsg"))) {
      if (grepl("Invited", sch$title)) {
        sch$type = "Invited talk"
        sch$talk_id = cells %>% html_node("a") %>% 
          html_attr("href") %>% 
          str_split("#") %>% 
          .[[1]] %>% .[2]
      } else if (grepl("Break", sch$title)) {
        sch$type = "Break"
      } else if (grepl("Lunch", sch$title)) {
        sch$type = "Lunch"
      } else if (grepl("Award", sch$title)) {
        sch$type = "Award"
      }
      sch$schedule_id = str_c(day, "-", time[1], "-", time[2])
    } else {
      sch$type = "Oral"
      sch$location = rooms
      sch$session_id = cells %>% 
        html_nodes("a") %>% 
        html_attr("href") %>% 
        str_replace("#", "")
      sch$schedule_id = str_c(day, "-", time[1], "-", time[2], "-", str_replace_all(rooms, "[ \\+]", "-"))
    }
  }
  
  schedule = schedule %>% 
    bind_rows(as_tibble(sch))
  
  last_time_end = sch$time_end
}

# increment session ids when using several time slots
schedule = schedule %>% 
  group_by(day, title, location, type) %>% 
  mutate(session_id = str_c(session_id, seq_len(n()), sep=" ")) %>% 
  ungroup()
  

# Detailed Schedule
#===============================

fix_affiliation  = function(x) {
  x[x == "The University of Hong Kong"] = "University of Hong Kong"
  x[x == "Baidu SVAIL"] = "Baidu USA, Inc."
  x[x == ""] = NA
  return(x)
}

parse_talk = function(html) {
  out = list()
  
  out$title = html %>% 
    html_node(".titlepaper a") %>% 
    html_text()
  
  out$paper_id = html %>% 
    html_node(".titlepaper") %>% 
    html_attr("id") %>% 
    as.integer()
  
  out$award = html %>% 
    html_node("b") %>% 
    html_text()
  
  out$session_time = html %>% 
    html_node(xpath = "text()") %>% 
    html_text() %>% 
    str_replace_all("[^[:digit:]:]", "")
  
  out$authors = html %>% 
    html_node(".authors") %>% 
    html_nodes(xpath = "text()") %>% 
    html_text() %>% 
    str_replace(",", "") %>% 
    str_replace_all("\\s+", " ") %>% 
    str_trim() %>% 
    list()
  
  out$affiliations = html %>% 
    html_nodes(".authors > i") %>% 
    html_text() %>% 
    str_replace_all("</?i>", "") %>% 
    fix_affiliation() %>% 
    list()
  
  out$pdf_url = html %>% 
    html_node(".lnks a") %>% 
    html_attr("href")
  
  out$filename = out$pdf_url %>% 
    tools::file_path_sans_ext() %>% 
    basename()
  
  out$review_url = str_c(reviews_url, out$paper_id,".txt")
  
  out$rebuttal_url = str_c(rebuttals_url, out$paper_id,".txt")
  
  out$poster_session = html %>% 
    html_nodes(".pos_ses") %>% 
    html_text()
  
  fn = file.path(data_path, "reviews", str_c(out$id,".txt"))
  if (dl_review && !file.exists(fn))
    download.file(out$review_url, fn)
  
  fn = file.path(data_path, "rebuttals", str_c(out$id,".txt"))
  if (dl_rebuttal && !file.exists(fn))
    download.file(out$rebuttal_url, fn)
  
  return(out)
}

html = read_html(schedule_url) %>% 
  html_nodes('h2:not([id~="main"]) ~ h3, h2:not([id~="main"]) ~ h4, h2:not([id~="main"]) ~ div, h2:not([id~="main"]) ~ li')
  
sessions_schedule = NULL
p = progress_estimated(length(html))

for (i in seq_along(html)) {
  id = html[i] %>% html_attr("id")
  nm = html[i] %>% html_name()
  txt = html[i] %>% html_text()
  
  if (nm == "h3") {
    spl = str_split(txt, " – ")[[1]]
    session_id = id
    session_day = spl[1]
    session_title = spl[2] %>% 
      str_replace_all("[[:blank:]]", " ")
  } else if (nm == "h4") {
    session_chair = txt %>% 
      str_split(":") %>% 
      .[[1]] %>% 
      .[2] %>% 
      str_trim()
  } else if (nm == "div") {
    session_location = html[i] %>% 
      html_node("b") %>% 
      html_text() %>% 
      str_trim()
  } else if (nm == "li") {
    out = parse_talk(html[i])
    out$session_id = session_id
    out$session_url = str_c(schedule_url, "#", session_id)
    out$session_title = session_title
    out$session_chair = session_chair
    out$session_day = session_day
    out$session_location = session_location
    
    if (is.null(sessions_schedule))
      sessions_schedule = as_tibble(out)
    else {
      sessions_schedule = sessions_schedule %>% 
        bind_rows(as_tibble(out))
    }
  }
  p$tick()$print()
}

# increment session ids when chair is sdifferent
session_ids = sessions_schedule %>% 
  select(session_id, session_day, session_location, session_title, session_chair) %>% 
  distinct() %>% 
  group_by(session_day, session_location, session_title) %>% 
  mutate(session_id = str_c(session_id, seq_len(n()), sep=" ")) %>% 
  ungroup()
  
sessions_schedule = sessions_schedule %>% 
  select(-session_id) %>% 
  left_join(session_ids, by = c("session_day", "session_location", "session_title", "session_chair"))

# Join tables
#=============

joined = papers %>% 
  left_join(sessions_schedule %>% 
              select(-pdf_url), 
            by = "filename") %>% 
  select(-title.y, -authors.x) %>% 
  rename(title = title.x, authors = authors.y)

authors = joined %>% 
  select(authors, affiliations, paper_id) %>% 
  unnest() %>% 
  rename(author = authors, affiliation = affiliations) %>% 
  group_by(author) %>% 
  arrange(desc(affiliation)) %>% 
  fill(affiliation) %>% 
  group_by(author, affiliation) %>% 
  summarise(paper_ids = list(paper_id)) %>% 
  group_by(author) %>% 
  mutate(author_id = str_to_lower(author) %>% 
           str_replace_all("\\s", "-") %>% 
           str_c(seq_len(n()), sep="-")) %>% 
  ungroup() %>% 
  select(author_id, everything())
  
paper_authors = authors %>% 
  select(author_id, paper_ids) %>% 
  unnest() %>% 
  rename(paper_id = paper_ids) %>% 
  group_by(paper_id) %>% 
  summarise(author_ids = list(author_id)) %>% 
  ungroup()

todate = function(session_day, session_time) {
  ind_day = match(session_day, c("Monday", "Tuesday", "Wednesday"))
  date = str_c("2016-06-", c("20", "21", "22"))[ind_day]
  ampm = if_else(session_time>"07:59" & session_time<"12:00", "AM", "PM")
  as.POSIXct(str_c(date, session_time, ampm, sep=" "),
             format ="%Y-%m-%d %I:%M %p")
}

sessions = joined %>% 
  select(session_id, session_day, session_title, session_chair, session_location, session_time, paper_id, session_url) %>% 
  mutate(session_time = todate(session_day, session_time)) %>% 
  arrange(session_time, session_title) %>% 
  group_by(session_id, session_day, session_title, session_chair, session_location, session_url) %>% 
  nest(.key = "talks") %>% 
  ungroup()

papers = joined %>% 
  left_join(sessions, by = c("session_id", "session_title", "session_chair", "session_day", "session_location", "session_url")) %>% 
  left_join(paper_authors, by="paper_id") %>% 
  select(paper_id, title, abstract, author_ids, filename, url, pdf_url, supp_pdf_url, 
       review_url, rebuttal_url, session_id, session_time, poster_session)


schedule %>% 
  left_join(invited_talks %>% 
              group_by(talk_id) %>% 
              nest(.key = "talk"),
            by = "talk_id") %>% 
  left_join(sessions %>% 
              group_by(session_id) %>% 
              nest(.key = "session"),
            by = c("session_id")) %>% 
  select(-talk_id, -session_id) %>% 
  View()

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

sessions %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(data_path, "sessions.json"))

invited_talks %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(data_path, "invited_talks.json"))
