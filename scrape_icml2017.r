#!/usr/bin/Rscript --slave

library(tidyverse, quietly=TRUE)
library(stringr, quietly=TRUE)
library(rvest, quietly=TRUE)
library(jsonlite, quietly=TRUE)

readRenviron(".env")

data_path = file.path(Sys.getenv("DATA_ROOT"), Sys.getenv("DATA_DIR"))
raw_path = file.path(data_path, "raw")
pdf_path = file.path(data_path, Sys.getenv("PDF_DIR"))

dir.create(raw_path, showWarnings = FALSE, recursive=TRUE)

papers_url = "http://proceedings.mlr.press/v70/"
schedule_url = "https://2017.icml.cc/Conferences/2017/Schedule"
keynotes_url = "https://2017.icml.cc/Conferences/2017/InvitedSpeakers"

# enable file downloads
dl_pdf = Sys.getenv("SCRAPE_DL_PDF") == "1"
dl_supp_pdf = Sys.getenv("SCRAPE_DL_SUPP_PDF") == "1"

if (dl_pdf || dl_supp_pdf)
  dir.create(pdf_path, showWarnings = FALSE, recursive = TRUE)

# Papers proceedings
#======================
parse_paper = function(url, .pb=NULL) {
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  html = read_html(url)
  
  out = list(paper_url = url)
  
  out$filename = out$paper_url %>% 
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
    str_replace(";", "") %>% 
    str_split(",") %>% 
    map(str_trim)
  
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
  
  fn = file.path(pdf_path, basename(out$pdf_url))
  if (dl_pdf && !file.exists(fn))
    download.file(out$pdf_url, fn)
  
  if (!is.na(out$supp_pdf_url) && nchar(out$supp_pdf_url)>0) {
    fn = file.path(pdf_path, basename(out$supp_pdf_url))
    if (dl_supp_pdf && !file.exists(fn))
      download.file(out$supp_pdf_url, fn)
  }
  
  return(out)
}

urls = read_html(papers_url) %>% 
  html_nodes("div.paper p.links a") %>% 
  html_attr("href") %>% 
  keep(~str_detect(.x, "^.*\\.html$"))

papers_proc = urls %>% 
  map_df(parse_paper, .pb = progress_estimated(length(urls)))

papers_proc %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(raw_path, "papers_proc.json"))

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

# Schedule (website)
# ====================

parse_schedule = function(html, .pb = NULL) {
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  out = list()
  
  out$event_id = html %>%
    html_attr("id") %>%
    str_extract("\\d+")
  
  pull_right = html %>% 
    html_nodes(".pull-right")
  
  out$type = pull_right[[1]] %>% 
    html_text(trim = TRUE)
  
  if (length(pull_right)>1) {
    a = pull_right[[2]] %>% 
      html_node("a")
    
    out$session = a %>% 
      html_text(trim = TRUE)
    
    out$session_id = a %>%
      html_attr("href") %>%
      str_split("=") %>%
      {.[[1]][2]}
  }
  
  time_location = html %>% 
    html_node(".maincardHeader:not(.pull-right)") %>% 
    html_text(trim = TRUE) %>%
    str_match("(.*?) +(.*?) +(.*?) +(.*?) +-- +(.*?) +(.*?) +(.*+)")
  
  out$day = str_c(time_location[2:4], collapse=" ")
  if (str_length(time_location[5]) < 6)
    out$time_start = str_c(time_location[5], time_location[7], sep=" ")
  else
    out$time_start = time_location[5]
  out$time_end = str_c(time_location[6], time_location[7], sep=" ")
  out$location = time_location[8] %>% 
    str_replace_all("@", "") %>% 
    str_trim()
  
  if (out$type == "Poster") {
    out$poster_location = out$location
    out$location = str_extract(out$location, "[^#]+") %>% str_trim()
  }
  
  out$title = html %>% 
    html_node(".maincardBody") %>% 
    html_text(trim = TRUE)
  out$authors = html %>% 
    html_node(".maincardFooter") %>% 
    html_text(trim = TRUE) %>% 
    str_split(" · ")
  out$paper_url = html %>% 
    html_node(".href_PDF") %>% 
    html_attr("href")
  return(out)
}

html = read_html(schedule_url) %>% 
  html_nodes("div.col-xs-12:nth-child(5) > div[onclick] > div[id]")

schedule = html %>% 
  map_df(parse_schedule, .pb = progress_estimated(length(html)))

schedule %>% 
  toJSON(pretty = TRUE) %>% 
  write(file.path(raw_path, "schedule.json"))

# Events (website)
#==============

parse_event = function(event_id, .pb = NULL) {
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  url = str_c(schedule_url, "/?showEvent=", event_id)
  
  out = list(event_id = event_id,
             event_url = url)
  
  html = read_html(url)
  
  btns = html %>% 
    html_nodes("button.btn[onclick]:not(.invisible)")
  
  speaker_ids = btns %>% 
    html_attr("onclick") %>% 
    str_extract("\\d+-\\d+")
  
  speaker_names = btns %>% 
    html_text(trim = TRUE) %>% 
    str_replace("»", "") %>% 
    str_trim()
  
  author_details = data_frame()
  if (length(speaker_ids)>0) {
    author_details = data_frame(author = speaker_names,
                                author_id = str_extract(speaker_ids, "\\d+"),
                                author_url = str_c(schedule_url, "/?showSpeaker=", speaker_ids))
  }
  
  out$author_details = list(author_details)
  
  out$abstract = html %>% 
    html_node(".abstractContainer") %>% 
    html_text(trim = TRUE)
  
  if(str_length(out$abstract)==0)
    out$abstract = NA_character_
  
  return(as_tibble(out))
}

events = schedule$event_id %>% 
{ map_df(., parse_event, .pb = progress_estimated(length(.))) }

events %>% 
  toJSON(pretty = TRUE) %>% 
  write(file.path(raw_path, "events.json"))

# Authors (website)
#==============

parse_author = function(url, .pb = NULL) {
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  html = read_html(url) %>% 
    html_node(".Remark")
  
  out = list()
  out$author = html %>% 
    html_node("h3") %>% 
    html_text(trim = TRUE) %>% 
    str_trim() %>% 
    {if_else(str_length(.)>0, ., NA_character_)}
  out$affiliation = html %>% 
    html_node("h4") %>% 
    html_text(trim = TRUE) %>% 
    {if_else(str_length(.)>0, ., NA_character_)}
  out$bio = html %>% 
    html_node("div[style]") %>% 
    html_text(trim = TRUE) %>% 
    {if_else(str_length(.)>0, ., NA_character_)}
  return(as_tibble(out))
}

authors = events %>% 
  select(author_details) %>% 
  unnest() %>% 
  group_by(author_id) %>% 
  summarize(author_url = first(author_url)) %>% 
  mutate(details = map(author_url, parse_author, .pb = progress_estimated(n()))) %>% 
  unnest(details)

authors %>% 
  toJSON(pretty = TRUE) %>% 
  write(file.path(raw_path, "authors.json"))


# # Invited talks (website)
# #===============================
# html = read_html(keynotes_url) %>% 
#   html_nodes("div.col-xs-12:nth-child(5) > div:nth-child(1) > div:nth-child(1)") %>% 
#   html_children()
# 
# i_talk = 0
# keynotes = NULL
# for (i in seq_along(html)) {
#   nm = html[[i]] %>% html_name()
#   txt = html[[i]] %>% html_text() %>% str_trim()
#   if (nm == "h3") {
#     if (i_talk>0) {
#       keynotes = keynotes %>% 
#         bind_rows(as_tibble(talk))
#     }
#     i_talk = i_talk+1
#     talk = list(title = txt)
#   }
#   if (nm == "h4") {
#     m = str_match(txt, "(.*?)\\s\\((.*?)\\)")
#     talk$speaker = m[2]
#     talk$affiliation = m[3]
#   }
#   if (nm == "p" && str_length(txt)>0) {
#     if (str_detect(txt, "^Abstract:"))
#       talk$abstract = txt
#     if (is.null(talk$bio))
#       talk$bio = txt
#     else
#       talk$bio = talk$bio %>% str_c(txt, sep = "\n")
#   }
# }
# 
# keynotes = keynotes %>% 
#   bind_rows(as_tibble(talk)) %>% 
#   mutate(keynote_speaker = speaker) %>% 
#   group_by(keynote_speaker) %>% 
#   nest(.key = "keynote")
