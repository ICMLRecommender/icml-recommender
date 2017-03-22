require(rvest)
require(tidyr)
require(dplyr)
require(jsonlite)

data_path = "data/icml2016"

dir.create(data_path, showWarnings = FALSE)

papers_url = "http://jmlr.org/proceedings/papers/v48/"
schedule_url = "http://icml.cc/2016/?page_id=1839"
reviews_url = "http://icml.cc/2016/reviews/"
rebuttals_url = "http://icml.cc/2016/rebuttals/"

# enable file downloads
dl_pdf = FALSE
dl_supp_pdf = FALSE
dl_review = FALSE
dl_rebuttal = FALSE

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
  
  out$key = out$url %>% 
    basename() %>% 
    tools::file_path_sans_ext()
  
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
    html_attr("href") %>% 
    paste0(papers_url, .)
  
  out$supp_pdf_url = html %>% 
    html_nodes("#extras ul li:nth-child(2) a") %>% 
    html_attr("href")
  
  
  if (length(out$supp_pdf_url)==0)
    out$supp_pdf_url = NA
  else
    out$supp_pdf_url = paste0(papers_url, out$supp_pdf_url)
  
  if (dl_pdf)
    download.file(out$pdf_url, file.path(data_path, "papers", out$pdf_url))
  
  if (dl_supp_pdf && nchar(out$supp_pdf_url)>0)
    download.file(out$supp_pdf_url, file.path(data_path, "papers", out$supp_pdf_url))
  
  return(tbl_df(out))
}

html = read_html(papers_url)

urls = html %>% 
  html_nodes("div.paper p.links a") %>% 
  html_attr("href")

papers = data_frame(url = urls) %>% 
  filter(grepl("^.*\\.html$", url)) %>% 
  mutate(url = paste0(papers_url, url)) %>% 
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
    as.numeric()
  
  out$session_time = html %>% 
    html_node(xpath = "text()") %>% 
    html_text() %>% 
    sub("–", "", .) %>% 
    trimws()
  
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
  
  out$review_url = paste0(reviews_url, out$paper_id,".txt")
  
  out$rebuttal_url = paste0(rebuttals_url, out$paper_id,".txt")
  
  out$poster_session = html %>% 
    html_nodes(".pos_ses") %>% 
    html_text()
  
  if (dl_review)
    download.file(out$review_url, file.path(data_path, "reviews", paste0(out$id,".txt")))
  
  if (dl_rebuttal)
    download.file(out$rebuttal_url, file.path(data_path, "rebuttals", paste0(out$id,".txt")))
  
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
  left_join(schedule, by = "pdf_url") %>% 
  select(-title.y, -authors.y) %>% 
  rename(title = title.x, authors = authors.x)

authors = joined %>% 
  select(authors, affiliations, paper_id) %>% 
  unnest() %>% 
  rename(author = authors, affiliation = affiliations) %>% 
  group_by(author) %>% 
  arrange(desc(affiliation)) %>% 
  fill(affiliation) %>% 
  group_by(author, affiliation) %>% 
  nest(.key = paper_ids) %>% 
  mutate(paper_ids = lapply(paper_ids, function(x) unlist(x, use.names = FALSE))) %>% 
  ungroup() %>% 
  mutate(author_id = 1:n()) %>% 
  select(author_id, everything())
  
paper_authors = authors %>% 
  select(author_id, paper_ids) %>% 
  unnest() %>% 
  rename(paper_id = paper_ids) %>% 
  group_by(paper_id) %>% 
  nest(.key = author_ids) %>% 
  mutate(author_ids = lapply(author_ids, function(x) unlist(x, use.names = FALSE))) %>% 
  ungroup()

sessions = joined %>% 
  select(session_title, session_day, session_chair, session_location, session_time, paper_id) %>% 
  group_by(session_title, session_day, session_chair, session_location) %>% 
  arrange(session_time) %>% 
  nest(.key = "talks") %>% 
  ungroup() %>% 
  mutate(session_id = 1:n()) %>% 
  select(session_id, everything())

papers = joined %>% 
  left_join(sessions, by = c("session_title", "session_chair", "session_day", "session_location")) %>% 
  left_join(paper_authors, by="paper_id") %>% 
  select(paper_id, title, abstract, author_ids, key, url, pdf_url, supp_pdf_url, 
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
