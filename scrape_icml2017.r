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

if (!is.null(cfg$scrape$zip_url)) {
  # download zip file with all the data
  
  zip_file = ".data.zip"
  download.file(cfg$scrape$zip_url, zip_file)
  unzip(zip_file, exdir=cfg$data$path)
  file.remove(zip_file)
  
} else {
  # scrape from website or other files
  
  papers_url = "http://proceedings.mlr.press/v70/"
  schedule_url = "https://2017.icml.cc/Conferences/2017/Schedule"
  keynotes_url = "https://2017.icml.cc/Conferences/2017/InvitedSpeakers"
  
  # enable file downloads
  dl_pdf = cfg$scrape$dl_pdf
  dl_supp_pdf = cfg$scrape$dl_supp_pdf
  dl_schedule = cfg$scrape$dl_schedule
  
  if (dl_pdf || dl_supp_pdf)
    dir.create(file.path(data_path, "papers"), showWarnings = FALSE)
  
  # Papers
  #======================
  parse_paper = function(url, .pb=NULL) {
    if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
    
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
    
    fn = file.path(data_path, "papers", basename(out$pdf_url))
    if (dl_pdf && !file.exists(fn))
      download.file(out$pdf_url, fn)
    
    if (!is.na(out$supp_pdf_url) && nchar(out$supp_pdf_url)>0) {
      fn = file.path(data_path, "papers", basename(out$supp_pdf_url))
      if (dl_supp_pdf && !file.exists(fn))
        download.file(out$supp_pdf_url, fn)
    }
    
    return(out)
  }
  
  urls = read_html(papers_url) %>% 
    html_nodes("div.paper p.links a") %>% 
    html_attr("href") %>% 
    keep(~str_detect(.x, "^.*\\.html$"))
  
  papers = urls %>% 
    map_df(parse_paper, .pb = progress_estimated(length(urls)))
  
  papers = papers %>% 
    mutate(paper_id = seq_len(n()))
  
  # Authors
  # ================
  authors = data_frame(author_id = numeric(0))
  
  # authors = papers %>% 
  #   select(paper_id, authors) %>% 
  #   unnest() %>% 
  #   rename(author = authors) %>% 
  #   group_by(author) %>% 
  #   nest(.key = paper_ids) %>% 
  #   mutate(paper_ids = paper_ids %>% map("paper_id"))
  
  # stem_fun = function(...) {
  #   str_c(..., sep=" ") %>% 
  #   str_to_lower() %>% 
  #     iconv(to='ASCII//TRANSLIT') %>% 
  #     str_replace_all("[^\\w]", "")
  # }
  # 
  # papers %>% 
  #   select(paper_id, title, authors) %>% 
  #   mutate(title_stem = stem_fun(title)) %>% 
  #   left_join(paper_authors %>% 
  #               ungroup() %>% 
  #               select(title, author_id, author) %>% 
  #               mutate(title_stem = stem_fun(title)) %>% 
  #               select(-title) %>% 
  #               nest(author_id, author, .key = "authors_"),
  #             by = "title_stem") %>% 
  #   filter(map_lgl(authors_, is.null)) %>% 
  #   View()
  
  # Schedule (website)
  # ====================
  parse_schedule = function(html, .pb = NULL) {
    if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
    
    out = list()
    # out$schedule_id = html %>% 
    #   html_attr("id") %>% 
    #   str_extract("\\d+")
    
    pull_right = html %>% 
      html_nodes(".pull-right")
    
    out$type = pull_right[[1]] %>% 
      html_text()
    
    if (length(pull_right)>1) {
      a = pull_right[[2]] %>% 
        html_node("a")
      
      out$session = a %>% 
        html_text() %>% 
        str_trim()
      
      out$session_id = a %>%
        html_attr("href") %>%
        str_split("=") %>%
        {.[[1]][2]}
    }
    
    time_location = html %>% 
      html_node(".maincardHeader:not(.pull-right)") %>% 
      html_text() %>%
      str_match("(.*?) +(.*?) +(.*?) +(.*?) +-- +(.*?) +(.*?) +@ +(.*+)")
    
    out$day = str_c(time_location[2:4], collapse=" ")
    if (str_length(time_location[5]) < 6)
      out$time_start = str_c(time_location[5], time_location[7], sep=" ")
    else
      out$time_start = time_location[5]
    out$time_end = str_c(time_location[6], time_location[7], sep=" ")
    out$location = time_location[8]
    
    if (out$type == "Poster") {
      out$poster_location = time_location[8]
      out$location = str_extract(time_location[8], "[^#]") %>% str_trim()
    }
    
    out$title = html %>% 
      html_node(".maincardBody") %>% 
      html_text()
    out$authors = html %>% 
      html_node(".maincardFooter") %>% 
      html_text() %>% 
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
  
  locale = Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "en_US.utf8")
  
  schedule = schedule %>% 
    mutate(session = if_else(is.na(session), title, session)) %>% 
    group_by(type, day, location, session, session_id) %>% 
    mutate(break_id = if_else(type == "Break", seq_len(n()), NA_integer_)) %>% 
    group_by(type, day, location, session, session_id, break_id) %>% 
    { mutate(ungroup(.), schedule_id = group_indices(.)) } %>% 
    ungroup() %>% 
    select(-session_id, -break_id) %>% 
    left_join(group_by(., schedule_id) %>% 
                summarize(session_time_start = first(time_start),
                          session_time_end = last(time_end)),
              by = "schedule_id") %>% 
    mutate(session_time = as.POSIXct(str_c(day, session_time_start), format = "%a %b %dth %I:%M %p"))
  
  Sys.setlocale(locale)
  
  # # Invited talks
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
  
  # Join tables
  # ===================
  
  talks_schedule = schedule %>% 
    filter(type %in% c("Talk")) %>% 
    select(schedule_id, time_start, time_end, paper_url) %>% 
    left_join(papers %>% 
                select(paper_id, url), 
              by = c("paper_url" = "url")) %>% 
    select(-paper_url)
  
  posters_schedule = schedule %>% 
    filter(type %in% c("Poster")) %>% 
    select(schedule_id, time_start, time_end, paper_url, poster_location) %>% 
    left_join(papers %>% 
                select(paper_id, url), 
              by = c("paper_url" = "url")) %>% 
    select(-paper_url)
  
  papers = papers %>%
    left_join(talks_schedule %>% 
                group_by(paper_id) %>% 
                nest(.key = "talk_schedule"),
              by = "paper_id") %>% 
    left_join(posters_schedule %>% 
                group_by(paper_id) %>% 
                nest(.key = "poster_schedule"),
              by = "paper_id")
  
  schedule = schedule %>% 
    left_join(bind_rows(talks_schedule, posters_schedule), 
              by = c("schedule_id", "time_start", "time_end", "poster_location")) %>% 
    select(-paper_url) %>% 
    group_by(schedule_id, type, day, location, session, session_time, session_time_start, session_time_end) %>% 
    nest(.key = "content") %>% 
    mutate(content = content %>% map_if(type %in% c("Talk", "Poster", "Break"), ~select(.x, -authors))) %>% 
    mutate(content = content %>% map_if(type %in% c("Talk", "Poster"), ~select(.x, -title))) %>% 
    mutate(content = content %>% map_if(!type %in% c("Talk", "Poster"), ~select(.x, -paper_id))) %>% 
    mutate(content = content %>% map_if(type != "Poster", ~select(.x, -poster_location))) %>% 
    mutate(content = content %>% map_if(type == "Talk" & map_int(content, nrow)==1, ~NA)) %>% 
    mutate(content = content %>% map_if(type == "Break", ~NA))
  
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
  
}
