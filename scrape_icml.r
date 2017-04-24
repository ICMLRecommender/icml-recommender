#!/usr/bin/Rscript --slave

require(rvest)
require(tidyr)
require(dplyr)
require(jsonlite)
require(yaml)

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
  
  out$mlr_paper_id = out$url %>% 
    tools::file_path_sans_ext() %>% 
    basename()
  
  out$title = html %>% 
    html_nodes("h1") %>% 
    html_text() %>% 
    # gsub("\\\\[()]", "$", .) %>% 
    trimws()
  
  out$authors = html %>% 
    html_nodes("#authors") %>% 
    html_text() %>% 
    trimws() %>% 
    strsplit("[,\n\t]+")
  
  out$abstract = html %>% 
    html_nodes("#abstract") %>% 
    html_text() %>% 
    # gsub("\\\\[()]", "$", .) %>% 
    trimws()
  
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
  
  return(tbl_df(out))
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

# Schedule
#===============================
fix_affiliations  = function(x) {
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
  
  out$session_time = html %>% 
    html_node(xpath = "text()") %>% 
    html_text() %>% 
    gsub("[^[:digit:]:]", "", .)
  
  out$authors = html %>% 
    html_node(".authors") %>% 
    html_nodes(xpath = "text()") %>% 
    html_text() %>% 
    sub(",", "", .) %>% 
    gsub("[[:blank:]]+", " ", .) %>% 
    trimws() %>% 
    list()
  
  out$affiliations = html %>% 
    html_nodes(".authors > i") %>% 
    html_text() %>% 
    gsub("</?i>", "", .) %>% 
    fix_affiliations() %>% 
    list()
  
  out$pdf_url = html %>% 
    html_node(".lnks a") %>% 
    html_attr("href")
  
  out$mlr_paper_id = out$pdf_url %>% 
    tools::file_path_sans_ext() %>% 
    basename()
  
  out$review_url = paste0(reviews_url, out$paper_id,".txt")
  
  out$rebuttal_url = paste0(rebuttals_url, out$paper_id,".txt")
  
  out$poster_session = html %>% 
    html_nodes(".pos_ses") %>% 
    html_text()
  
  fn = file.path(data_path, "reviews", paste0(out$id,".txt"))
  if (dl_review && !file.exists(fn))
    download.file(out$review_url, fn)
  
  fn = file.path(data_path, "rebuttals", paste0(out$id,".txt"))
  if (dl_rebuttal && !file.exists(fn))
    download.file(out$rebuttal_url, fn)
  
  return(out)
}

html = read_html(schedule_url) %>% 
  html_node("#schedule") %>% 
  html_children()

schedule = NULL
start = FALSE
p = progress_estimated(length(html))
for (i in seq_along(html)) {
  id = html[i] %>% html_attr("id")
  
  if (!is.na(id) && id == "main") start = TRUE
  if (!start) next()
  
  nm = html[i] %>% html_name()
  txt = html[i] %>% html_text()
  
  if (nm == "h3") {
    spl = strsplit(txt, " – ")[[1]]
    session_id = id
    session_day = spl[1]
    session_title = spl[2] %>% 
      gsub("[[:blank:]]", " ", .)
  }
  if (nm == "h4") {
    session_chair = txt %>% 
      strsplit(":") %>% 
      .[[1]] %>% 
      .[2] %>% 
      trimws()
  }
  if (nm == "div") {
    session_location = html[i] %>% 
      html_node("b") %>% 
      html_text() %>% 
      trimws()
  }
  if (nm == "li") {
    out = parse_talk(html[i])
    out$session_id = session_id
    out$session_title = session_title
    out$session_chair = session_chair
    out$session_day = session_day
    out$session_location = session_location
    
    if (is.null(schedule))
      schedule = tbl_df(out)
    else {
      schedule = schedule %>% 
        bind_rows(tbl_df(out))
    }
  }
  p$tick()$print()
}

# Join tables
#=============

joined = papers %>% 
  left_join(schedule %>% 
              select(-pdf_url), 
            by = "mlr_paper_id") %>% 
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
  mutate(author_id = paste(gsub(" ", "-", tolower(author)), seq_len(n()), sep="-")) %>% 
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
  date = paste0("2016-06-", c("20", "21", "22"))[ind_day]
  ampm = ifelse(session_time>"07:59" & session_time<"12:00", "AM", "PM")
  as.POSIXct(paste(date, session_time, ampm),
             format ="%Y-%m-%d %I:%M %p")
}

sessions = joined %>% 
  select(session_id, session_day, session_title, session_chair, session_location, session_time, paper_id) %>% 
  mutate(session_time = todate(session_day, session_time)) %>% 
  arrange(session_time, session_title) %>% 
  group_by(session_id, session_day, session_title, session_chair, session_location) %>% 
  nest(.key = "talks") %>% 
  ungroup()

papers = joined %>% 
  left_join(sessions, by = c("session_id", "session_title", "session_chair", "session_day", "session_location")) %>% 
  left_join(paper_authors, by="paper_id") %>% 
  select(paper_id, title, abstract, author_ids, mlr_paper_id, url, pdf_url, supp_pdf_url, 
       review_url, rebuttal_url, session_id, session_time, poster_session)

# Write json
#============
papers %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(data_path, "papers.json"))

authors %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(data_path, "authors.json"))

sessions %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(data_path, "sessions.json"))

