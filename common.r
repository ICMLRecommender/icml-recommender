
library(tidyverse, quietly=TRUE)
library(stringr, quietly=TRUE)
library(rvest, quietly=TRUE)
library(jsonlite, quietly=TRUE)
library(sofa, quietly=TRUE)
library(magrittr, quietly=TRUE)

# Read environment variables
#=============================================
readRenviron(".env")

data_path = file.path(Sys.getenv("DATA_ROOT"), Sys.getenv("DATA_DIR"))
suffix = paste0("_", Sys.getenv("DATA_DIR"))
raw_path = file.path(data_path, "raw")
pdf_path = file.path(data_path, Sys.getenv("PDF_DIR"))
txt_path = file.path(data_path, Sys.getenv("TXT_DIR"))
abstracts_txt_path = file.path(data_path, Sys.getenv("ABSTRACTS_TXT_DIR"))
vocab_path = file.path(txt_path, Sys.getenv("VOCAB_FILE"))
lda_output_path = file.path(data_path, Sys.getenv("LDA_OUTPUT_DIR"))
files_path = file.path(txt_path, Sys.getenv("FILES_FILE"))
mult_path = file.path(txt_path, Sys.getenv("MULT_FILE"))
ctr_output_path = file.path(data_path, Sys.getenv("CTR_OUTPUT_DIR"))
stopwords_path = file.path(raw_path, Sys.getenv("STOPWORDS_FILE"))

dl_pdf = Sys.getenv("SCRAPE_DL_PDF") == "1"
dl_supp_pdf = Sys.getenv("SCRAPE_DL_SUPP_PDF") == "1"

lda_n_topics = as.integer(Sys.getenv("LDA_N_TOPICS"))
lda_alpha = as.numeric(Sys.getenv("LDA_ALPHA"))

session_labels_url = Sys.getenv("SESSION_LABELS_CSV_URL")

subject_labels_url = Sys.getenv("SUBJECT_LABELS_CSV_URL")

topic_labels_url = Sys.getenv("TOPICS_LABELS_CSV_URL")

thres_weight_topic = as.numeric(Sys.getenv("TOPICS_THRES_TOPIC"))
thres_weight_word = as.numeric(Sys.getenv("TOPICS_THRES_WORD"))

couchdb_args = list(host = Sys.getenv("COUCHDB_HOST"),
                    port = Sys.getenv("COUCHDB_PORT"),
                    path = Sys.getenv("COUCHDB_PATH"),
                    transport = Sys.getenv("COUCHDB_TRANSPORT"),
                    user = Sys.getenv("COUCHDB_USER"),
                    pwd = Sys.getenv("COUCHDB_PWD"))

couchdb_revs_limit = as.integer(Sys.getenv("COUCHDB_REVS_LIMIT"))

simu_seed = as.integer(Sys.getenv("SIMU_SEED"))
simu_n_likes = as.integer(Sys.getenv("SIMU_N_LIKES"))

reco_n_top = as.integer(Sys.getenv("RECO_N_TOP"))

trending_dbname = Sys.getenv("TRENDING_DBNAME")
trending_docid = Sys.getenv("TRENDING_DOCID")
trending_field = Sys.getenv("TRENDING_FIELD")
trending_n_top = as.integer(Sys.getenv("TRENDING_N_TOP"))

trending_bookmark_weight = as.numeric(Sys.getenv("TRENDING_BOOKMARK_WEIGHT"))
trending_like_weight = as.numeric(Sys.getenv("TRENDING_LIKE_WEIGHT"))


# web scraping functions
#=============================================

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
    str_replace_all("[@,]", "") %>% 
    str_trim()
  
  out$title = html %>% 
    html_node(".maincardBody") %>% 
    html_text(trim = TRUE)
  out$authors = html %>% 
    html_node(".maincardFooter") %>% 
    html_text(trim = TRUE) %>% 
    str_split(" · ")
  out$paper_url = html %>%
    html_node('.href_PDF[title="Paper"]') %>%
    html_attr("href")
  out$poster_url = html %>%
    html_node('.href_PDF[title="Poster"]') %>%
    html_attr("href")
  out$code_url = html %>%
    html_node('.href_Code[title="Code"]') %>%
    html_attr("href")
  out$video_url = html %>%
    html_node('.href_Video[title="Video Presentation"]') %>%
    html_attr("href")
  return(out)
}


parse_event = function(event_id, schedule_url, .pb = NULL) {
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  url = str_c(schedule_url, "?showEvent=", event_id)
  
  html = read_html(url)
 
  homepage_url = html %>% 
    html_node('div[style="text-align:center"]>a.btn') %>% 
    html_attr("href")
  
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
                                author_url = str_c(schedule_url, "?showSpeaker=", speaker_ids))
  }
  
  out = list(event_id = event_id,
             event_url = url,
             paper_url = paper_url,
             poster_url = poster_url,
             code_url = code_url,
             video_url = video_url,
             homepage_url = homepage_url)
  
  out$author_details = list(author_details)
  
  out$abstract = html %>% 
    html_node(".abstractContainer") %>% 
    html_text(trim = TRUE)
  
  if(str_length(out$abstract)==0)
    out$abstract = NA_character_
  
  event_schedule = html %>% 
    html_nodes("table td") %>% 
    html_text(trim = TRUE) %>% 
    matrix(ncol=2, byrow = TRUE) %>% 
    as_tibble() %>% 
    setNames(c("time", "title"))
  
  if (nrow(event_schedule)>0)
    out$event_schedule = list(event_schedule)
  
  return(as_tibble(out))
}


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


parse_subject = function(subject_id, schedule_url, .pb = NULL) {
  if ((!is.null(.pb)) && inherits(.pb, "Progress") && (.pb$i < .pb$n)) .pb$tick()$print()
  
  url = str_c(schedule_url, "?bySubject=&selectedSubject=", subject_id)
  
  html = read_html(url) %>% 
    html_nodes("div.col-xs-12.col-sm-9 > div[onclick] > div[id]")
  
  event_ids = html %>%
    html_attr("id") %>%
    str_extract("\\d+")
  
  return(data_frame(event_id = event_ids))
}

# Couchdb helper functions
#===========================================================

# set revisions limit
db_revs_limit = function(cdb, dbname, revs_limit) {
  httr::PUT(paste(cdb$make_url(), dbname, "_revs_limit", sep="/"), 
            cdb$get_headers(), body=as.character(revs_limit))
}

# request compaction
db_compact = function(cdb, dbname) {
  httr::POST(paste(cdb$make_url(), dbname, "_compact", sep="/"), 
             cdb$get_headers(), httr::content_type_json())
}


# write database
db_write = function(cdb, dbname, data, id, 
                    revs_limit = Sys.getenv("COUCHDB_REVS_LIMIT")) {
  id = enquo(id)
  if (dbname %in% db_list(cdb))
    cdb %>% db_delete(dbname=dbname)
  cdb %>% db_create(dbname)
  if (nchar(revs_limit)>0) {
    cdb %>% db_revs_limit(dbname, revs_limit)
  }
  if (!quo_name(id) %in% names(data)) {
    warning("missing ", quo_name(id), " in data")
    return(NULL)
  }
  docs = data %>%
    mutate(`_id` = str_replace_all(!!id, "[\\s\\+]" , "-")) %>% 
    split(.$`_id`) %>% 
    map_chr(~str_extract(toJSON(.x), "\\{.+\\}"))
  cdb %>% db_bulk_create(dbname, docs)
}


# generate random user likes
#======================================
sample_userlikes = function(userids, filenames,
                            n_likes = as.integer(Sys.getenv("SIMU_N_LIKES")),
                            seed = as.integer(Sys.getenv("SIMU_SEED"))) {
  
  if (!is.na(seed)) {
    old_seed = .Random.seed
    set.seed(seed)
  }
  
  userlikes = data_frame(user = sample(userids, n_likes, replace = TRUE), 
                         filename = sample(filenames, n_likes, replace = TRUE))
  
  if (!is.na(seed)) {
    .Random.seed = old_seed
  }
  return(userlikes)
}
