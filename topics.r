#!/usr/bin/Rscript --slave

library(tidyverse)
library(yaml)
library(jsonlite)
library(magrittr)
library(stringr)
library(knitr)

args = commandArgs(TRUE)

cfg_file = "config.yml"
if (length(args>0))
  cfg_file = args[1]

cfg = yaml.load_file(cfg_file)

data_path = cfg$data$path
txt_path = cfg$data$txt_path
files_path = cfg$data$files_path
vocab_path = cfg$data$vocab_path
lda_output_path = cfg$lda$output_path

thres_weight_topic = as.numeric(cfg$topics$thres_topic)
thres_weight_word = as.numeric(cfg$topics$thres_word)

# papers topic proportions
# =============================
gamma = read_delim(file.path(lda_output_path, "final.gamma"), 
                   delim=" ", col_names = FALSE,
                   col_types = cols(.default = "d"))

topic_weights = gamma %>% 
  colSums() %>% 
  divide_by(sum(.))

gamma = gamma %>% 
  sweep(1, rowSums(.), "/")

n_topics = ncol(gamma)
topic_ids = seq_len(n_topics)

colnames(gamma) = topic_ids

papers = fromJSON(file(file.path(data_path, "papers.json"))) %>% 
  as_tibble()

filenames = readLines(files_path) %>%
  tools::file_path_sans_ext() %>% 
  basename()

# add topic proportions to papers
paper_topics = gamma %>% 
  mutate(filename = filenames) %>% 
  mutate_at(vars(-filename), funs(if_else(.>thres_weight_topic, ., NA_character_))) %>% 
  gather(topic_id, weight, -filename, na.rm = TRUE) %>% 
  mutate(topic_id = as.integer(topic_id)) %>% 
  arrange(filename, desc(weight)) %>% 
  group_by(filename) %>% 
  nest(.key = "topics")

papers = papers %>% 
  left_join(paper_topics, by = "filename")

# add top papers to topics
topic_papers = gamma %>% 
  mutate(filename = filenames) %>% 
  mutate_at(vars(-filename), funs(if_else(.>thres_weight_topic, ., NA_character_))) %>% 
  gather(topic_id, weight, -filename, na.rm = TRUE) %>% 
  mutate(topic_id = as.integer(topic_id)) %>% 
  arrange(topic_id, desc(weight)) %>% 
  left_join(papers %>% 
              select(paper_id, title, filename), 
            by="filename") %>% 
  select(topic_id, paper_id, weight) %>% 
  group_by(topic_id) %>% 
  nest(.key = "papers")


# topics word proportions
# =======================
words = readLines(vocab_path)

beta = read_delim(file.path(lda_output_path, "final.beta"), 
                  delim=" ", 
                  col_types = cols(X1 = col_skip(), .default = "d"), 
                  col_names = c("X1", words)) %>%
  exp()

beta[beta<thres_weight_word] = NA

topics = beta %>% 
  mutate(topic_id = topic_ids) %>% 
  gather(word, weight, -topic_id, na.rm = TRUE) %>% 
  group_by(topic_id) %>% 
  arrange(desc(weight)) %>% 
  nest(.key = "words") %>% 
  arrange(topic_id) %>% 
  select(topic_id, words)

topics = topics %>% 
  mutate(weight = topic_weights) %>% 
  arrange(desc(weight))

topics = topics %>% 
  left_join(topic_papers, by="topic_id")

# write topics.md
#===================

print_topic = function(df) {
  out = c(str_c("# [", format(df$weight*100, digit=3), "%] topic ", df$topic_id))
  out = c(out, kable(df$words[[1]][1:10,]), "\n")
  out = c(out, kable(df$papers[[1]][1:5,]), "\n")
}

fc = file(file.path(data_path, "topics.md"))

topics %>%
  arrange(desc(weight)) %>% 
  split(., seq_len(nrow(.))) %>% 
  sapply(print_topic) %>% 
  writeLines(fc)

close(fc)

# Topic clusters
#================

topic_clusters = topics %>% 
  mutate(label = map_chr(words, ~str_c(.x$word[1:3], collapse="-"))) %>% 
  group_by(label) %>% 
  summarise(topic_ids = list(unique(topic_id)),
            weight = sum(weight),
            words = list(bind_rows(words)),
            papers = list(bind_rows(papers))) %>% 
  arrange(desc(weight)) %>% 
  mutate(topic_cluster_id = seq_len(n())) %>% 
  select(topic_cluster_id, everything())

topics = topics %>% 
  left_join(topic_clusters %>% 
              select(topic_cluster_id, topic_ids) %>% 
              unnest() %>% 
              rename(topic_id = topic_ids), 
            by = "topic_id")

# # clustering of topics
# #========================
# n_clust = 20
# 
# km_fit = kmeans(beta, n_clust)
# 
# # require(tsne)
# # require(ggplot2)
# # 
# # tsne_fit = tsne(beta, perplexity = 5)
# # 
# # tsne_fit %>% as_tibble() %>%
# #   ggplot(aes(V1, V2)) +
# #   geom_text(aes(label=topics$top_words,
# #                 col = as.factor(km_fit$cluster)),
# #             lineheight=.5,
# #             fontface = "bold")
# 
# topics = topics %>%
#   mutate(topic_cluster_id = km_fit$cluster)
# 
# subtopics = topics %>%
#   group_by(topic_cluster_id) %>%
#   summarise(topic_ids = list(topic_id))
# 
# weight_thres = 5e-3
# topic_clusters = beta %>%
#   mutate(topic_cluster_id = km_fit[["cluster"]]) %>%
#   group_by(topic_cluster_id) %>%
#   summarise_all(funs(mean)) %>%
#   mutate_at(vars(-topic_cluster_id), funs(if_else(.>weight_thres, ., NA_character_))) %>%
#   gather(word, weight, -topic_cluster_id, na.rm = TRUE) %>%
#   group_by(topic_cluster_id) %>%
#   arrange(desc(weight)) %>%
#   nest(.key = "words") %>%
#   mutate(label = map_chr(words, ~paste(.x$word[1:3], collapse=" "))) %>%
#   left_join(subtopics, by="topic_cluster_id")


# write json
#============

topics %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(data_path, "topics.json"))

topic_clusters %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(data_path, "topic_clusters.json"))

papers %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(data_path, "papers_topics.json"))
