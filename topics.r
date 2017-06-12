#!/usr/bin/Rscript --slave

require(tidyr)
require(dplyr)
require(readr)
require(yaml)
require(jsonlite)
require(magrittr)

args = commandArgs(TRUE)

cfg_file = "config.yml"
if (length(args>0))
  cfg_file = args[1]

cfg = yaml.load_file(cfg_file)

data_path = cfg$data$path
txt_path = cfg$data$txt_path
lda_output_path = cfg$lda$output_path


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

papers = fromJSON(file(file.path(data_path, "papers.json"))) %>% tbl_df() 

mlr_paper_ids = readLines(file.path(txt_path, "files.dat")) %>%
  tools::file_path_sans_ext() %>% 
  basename()

# add topic proportions to papers
thres_weight_topic = .1

paper_topics = gamma %>% 
  mutate(mlr_paper_id = mlr_paper_ids) %>% 
  mutate_at(vars(-mlr_paper_id), funs(ifelse(.>thres_weight_topic, ., NA))) %>% 
  gather(topic_id, weight, -mlr_paper_id, na.rm = TRUE) %>% 
  mutate(topic_id = as.integer(topic_id)) %>% 
  arrange(mlr_paper_id, desc(weight)) %>% 
  group_by(mlr_paper_id) %>% 
  nest(.key = "topics")

papers = papers %>% 
  left_join(paper_topics, by = "mlr_paper_id")

# add top papers to topics
topic_papers = gamma %>% 
  mutate(mlr_paper_id = mlr_paper_ids) %>% 
  mutate_at(vars(-mlr_paper_id), funs(ifelse(.>thres_weight_topic, ., NA))) %>% 
  gather(topic_id, weight, -mlr_paper_id, na.rm = TRUE) %>% 
  mutate(topic_id = as.integer(topic_id)) %>% 
  arrange(topic_id, desc(weight)) %>% 
  left_join(select(papers, mlr_paper_id, title), by="mlr_paper_id") %>% 
  group_by(topic_id) %>% 
  nest(.key = "papers")


# topics word proportions
# =======================
words = readLines(file.path(txt_path, "vocab.dat"))

beta = read_delim(file.path(lda_output_path, "final.beta"), 
                  delim=" ", 
                  col_types = cols(X1 = col_skip(), .default = "d"), 
                  col_names = c("X1", words)) %>%
  mutate_all(funs(exp))

thres_weight_word = 5e-3

topics = beta %>% 
  mutate(topic_id = topic_ids) %>% 
  mutate_at(vars(-topic_id), funs(ifelse(.>thres_weight_word, ., NA))) %>% 
  gather(word, weight, -topic_id, na.rm = TRUE) %>% 
  group_by(topic_id) %>% 
  arrange(desc(weight)) %>% 
  nest(.key = "words") %>% 
  arrange(topic_id) %>% 
  mutate(label = sapply(words, function(x) paste(x$word[1:3], collapse="-"))) %>% 
  select(topic_id, label, words)

topics = topics %>% 
  mutate(weight = topic_weights) %>% 
  arrange(desc(weight))

topics = topics %>% 
  left_join(topic_papers, by="topic_id")

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
# # tsne_fit %>% tbl_df() %>%
# #   ggplot(aes(V1, V2)) +
# #   geom_text(aes(label=topics$top_words,
# #                 col = as.factor(km_fit$cluster)),
# #             lineheight=.5,
# #             fontface = "bold")
# 
# topics = topics %>%
#   mutate(parent_topic_id = km_fit$cluster)
# 
# children_topics = topics %>%
#   group_by(parent_topic_id) %>%
#   summarise(topic_ids = list(topic_id))
# 
# weight_thres = 5e-3
# parent_topics = beta %>%
#   mutate(parent_topic_id = km_fit[["cluster"]]) %>%
#   group_by(parent_topic_id) %>%
#   summarise_all(funs(mean)) %>%
#   mutate_at(vars(-parent_topic_id), funs(ifelse(.>weight_thres, ., NA))) %>%
#   gather(word, weight, -parent_topic_id, na.rm = TRUE) %>%
#   group_by(parent_topic_id) %>%
#   arrange(desc(weight)) %>%
#   nest(.key = "words") %>%
#   mutate(top_words = sapply(words, function(x) paste(x$word[1:3], collapse=" "))) %>%
#   left_join(children_topics, by="parent_topic_id")


# write json
#============

topics %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(data_path, "topics.json"))

papers %>% 
  toJSON(pretty=TRUE) %>% 
  write(file.path(data_path, "papers_topics.json"))


# write topics.md
#===================

print_topic = function(df) {
  out = c(paste0("# [",format(df$weight*100, digit=3), "%] topic ", df$topic_id, ": ", df$label))
  out = c(out, knitr::kable(df$words[[1]][1:10,]), "\n")
  out = c(out, knitr::kable(df$papers[[1]][1:5,]), "\n")
}

topics %>%
  arrange(desc(weight)) %>% 
  split(., seq_len(nrow(.))) %>% 
  sapply(print_topic) %>% 
  writeLines(file(file.path(data_path, "topics.md")))
