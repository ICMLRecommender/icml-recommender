#!/usr/bin/Rscript --slave

source("common.r")

papers = fromJSON(file.path(data_path, "papers.json")) %>% 
  as_tibble()

filenames = readLines(files_path) %>%
  tools::file_path_sans_ext() %>% 
  basename()

# papers topic proportions
# =============================
topic_ids = as.character(seq_len(lda_n_topics))

gamma = read_delim(file.path(lda_output_path, "final.gamma"), 
                   delim=" ", col_names = topic_ids,
                   col_types = cols(.default = "d"))

topic_weights = gamma %>% 
  summarise_all(funs(sum)) %>% 
  gather(topic_id, topic_sum) %>% 
  mutate(topic_weight = topic_sum/sum(topic_sum))

paper_weights = data_frame(filename = filenames,
                           paper_sum = gamma %>% 
                             rowSums()) %>% 
  mutate(paper_weight = paper_sum/sum(paper_sum))

# add topic proportions to papers
paper_topics = gamma %>% 
  mutate(filename = filenames) %>% 
  gather(topic_id, gamma, -filename, na.rm = TRUE) %>% 
  left_join(paper_weights, by = "filename") %>% 
  mutate(weight = gamma/paper_sum)

paper_topics_nested = paper_topics %>% 
  select(filename, topic_id, weight) %>% 
  group_by(filename) %>% 
  arrange(desc(weight)) %>% 
  nest(.key = "topics")

topic_papers_nested = paper_topics %>% 
  select(topic_id, filename, weight) %>% 
  left_join(papers %>% 
              select(paper_id, filename), 
            by="filename") %>% 
  select(-filename) %>% 
  group_by(topic_id) %>% 
  arrange(desc(weight)) %>% 
  nest(.key = "papers")

# subject topic proportions
# =============================
subject_names = papers %>% 
  select(subjects) %>% 
  filter(subjects %>% map_lgl(is.data.frame)) %>% 
  unnest() %>% 
  distinct()

subject_topics = papers %>% 
  select(filename, subjects) %>% 
  filter(subjects %>% map_lgl(is.data.frame)) %>% 
  mutate(subjects = map(subjects, ~mutate(., weight=1/nrow(.)))) %>% 
  unnest(subjects) %>% 
  left_join(paper_topics %>% 
              select(filename, topic_id, gamma),
            by="filename") %>% 
  group_by(topic_id, subject_id) %>%
  summarise(gamma = sum(weight*gamma))

subject_weights = subject_topics %>% 
  group_by(subject_id) %>% 
  summarise(subject_sum = sum(gamma)) %>% 
  ungroup() %>% 
  mutate(subject_weight = subject_sum/sum(subject_sum))

subject_topics = subject_topics %>% 
  left_join(topic_weights, by = "topic_id") %>% 
  left_join(subject_weights, by = "subject_id") %>% 
  mutate(weight = gamma/subject_sum)

topic_subjects_nested = subject_topics %>% 
  select(topic_id, subject_id, weight) %>% 
  group_by(topic_id) %>% 
  arrange(desc(weight)) %>% 
  nest(.key = "subjects")

# topics term proportions
# =======================
terms = readLines(vocab_path)

beta = read_delim(file.path(lda_output_path, "final.beta"), 
                  delim=" ", 
                  col_types = cols(X1 = col_skip(), .default = "d"), 
                  col_names = c("X1", terms)) %>%
  exp()

topic_terms = beta %>% 
  mutate(topic_id = topic_ids) %>% 
  gather(term, beta, -topic_id, na.rm = TRUE) %>% 
  left_join(topic_weights, by = "topic_id") %>% 
  rename(weight = beta)

topic_terms_nested = topic_terms %>%
  select(topic_id, term, weight) %>% 
  group_by(topic_id) %>% 
  arrange(desc(weight)) %>% 
  nest(.key = "terms")

topics = topic_weights %>% 
  arrange(desc(topic_weight)) %>% 
  left_join(topic_terms_nested, by="topic_id") %>% 
  left_join(topic_papers_nested, by="topic_id") %>% 
  left_join(topic_subjects_nested, by="topic_id")


# Topic clusters
#================

############### TEMP #####
# topic_labels_file = file.path(raw_path, "topic_labels.csv")
# 
# download.file(topic_labels_url, topic_labels_file)
# 
# topic_labels = read_delim(topic_labels_file, delim = ",", trim_ws = TRUE) %>%
#   select(topic_id, labels) %>%
#   mutate(labels = str_split(labels, "/") %>%
#            map(str_trim))

topic_clusters = topics %>%
  mutate(labels = map(terms, ~.$term[1:2])) %>%  ############### TEMP #####
# left_join(topic_labels, by = "topic_id") %>% ############### TEMP #####
unnest(labels, .drop = FALSE) %>%
  rename(label = labels) %>%
  filter(!is.na(label)) %>%
  group_by(label) %>%
  summarise(topic_ids = list(unique(topic_id)),
            topic_cluster_weight = sum(topic_weight),
            terms = list(map2(terms, topic_cluster_weight, ~mutate(.x, weight = weight * .y)) %>%
                           bind_rows() %>%
                           mutate(weight = weight / sum(weight)) %>%
                           group_by(term) %>%
                           summarise(weight = sum(weight)) %>%
                           arrange(desc(weight))),
            papers = list(bind_rows(papers) %>%
                            group_by(paper_id) %>%
                            summarise(weight = sum(weight)) %>%
                            arrange(desc(weight)))) %>%
  arrange(desc(topic_cluster_weight)) %>%
  mutate(topic_cluster_id = seq_len(n())) %>%
  select(topic_cluster_id, everything())

topics = topics %>%
  left_join(topic_clusters %>%
              select(topic_cluster_id, topic_ids) %>%
              unnest() %>%
              rename(topic_id = topic_ids) %>%
              group_by(topic_id) %>%
              summarise(topic_cluster_ids = list(topic_cluster_id)),
            by = "topic_id")

# write json
#============
papers = papers %>% 
  left_join(paper_topics_nested, by = "filename")

papers %>%
  toJSON(pretty=TRUE) %>%
  write(file.path(data_path, "papers_topics.json"))

topics %>%
  toJSON(pretty=TRUE) %>%
  write(file.path(data_path, "topics.json"))

topic_clusters %>%
  toJSON(pretty=TRUE) %>%
  write(file.path(data_path, "topic_clusters.json"))


# write topics.md
#===================
if (require(knitr)) {
  
  print_topic = function(df, n=10) {
    out = c(str_c("# [", format(df$topic_weight*100, digit=3), "%] topic ", df$topic_id))
    terms = df$terms[[1]] %>% 
      arrange(desc(weight)) %>% 
      select(weight, term) %>% 
      slice(seq_len(n))
    out = c(out, knitr::kable(terms), "\n")
    papers = df$papers[[1]] %>% 
      arrange(desc(weight)) %>% 
      select(weight, title) %>% 
      slice(seq_len(n))
    out = c(out, knitr::kable(papers), "\n")
    subjects = df$subjects[[1]] %>% 
      arrange(desc(weight)) %>% 
      select(weight, subject) %>% 
      slice(seq_len(n))
    out = c(out, knitr::kable(subjects), "\n")
  }
  
  paper_titles = papers %>% 
    select(paper_id, title)
  
  fc = file(file.path(data_path, paste0("topics_k", lda_n_topics, "_alpha", lda_alpha, ".md")))
  
  topics %>%
    group_by(topic_id) %>% 
    do(mutate(., papers = map(papers, ~left_join(., paper_titles, by = "paper_id")))) %>% 
    do(mutate(., subjects = map(subjects, ~left_join(., subject_names, by = "subject_id")))) %>% 
    split(seq_len(nrow(.))) %>% 
    sapply(print_topic) %>% 
    writeLines(fc)
  
  close(fc)
}

