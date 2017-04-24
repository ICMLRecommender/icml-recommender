#!/usr/bin/Rscript --slave

require(tidyr)
require(dplyr)
require(readr)
require(yaml)

args = commandArgs(TRUE)

cfg_file = "config.yml"
if (length(args>0))
  cfg_file = args[1]

cfg = yaml.load_file(cfg_file)

data_path = cfg$data$path
txt_path = cfg$pdfconversion$txt_path
lda_output_path = cfg$lda$output_path

# topics
words = readLines(file.path(txt_path, "vocab.dat"))

beta = read_delim(file.path(lda_output_path, "final.beta"), 
                      delim=" ", 
                      col_types = cols(X1 = col_skip(), .default = "d"), 
                      col_names = c("X1", words)) %>%
  mutate_all(funs(exp))

n_topics = nrow(beta)

weight_thres = 5e-3

topics = beta %>% 
  mutate(topic_id = seq_len(n())) %>% 
  mutate_at(vars(-topic_id), funs(ifelse(.>weight_thres, ., NA))) %>% 
  gather(word, weight, -topic_id, na.rm = TRUE) %>% 
  group_by(topic_id) %>% 
  arrange(desc(weight)) %>% 
  nest(.key = "words") %>% 
  arrange(topic_id) %>% 
  mutate(topic_id = paste(sprintf("%03d", topic_id), 
                          sapply(words, function(x) paste(x$word[1:3], collapse="-")), sep = "-")) 

topic_ids = topics$topic_id

# papers topic proportions
# =============================
mlr_paper_ids = readLines(file.path(txt_path, "files.dat")) %>%
  tools::file_path_sans_ext() %>% 
  basename() 

gamma = read_delim(file.path(lda_output_path, "final.gamma"), 
                          delim=" ", col_names = topic_ids,
                   col_types = cols(.default = "d")) %>% 
  sweep(., 1, rowSums(.), "/")

papers = fromJSON(file(file.path(data_path, "papers.json"))) %>% tbl_df() 

# add topic proportions to papers
weight_thres = .1
paper_topics = gamma %>% 
  mutate(mlr_paper_id = mlr_paper_ids) %>% 
  mutate_at(vars(-mlr_paper_id), funs(ifelse(.>weight_thres, ., NA))) %>% 
  gather(topic_id, weight, -mlr_paper_id, na.rm = TRUE) %>% 
  arrange(mlr_paper_id, desc(weight)) %>% 
  group_by(mlr_paper_id) %>% 
  nest(.key = "topics")

papers = papers %>% 
  left_join(paper_topics, by = "mlr_paper_id")

# add top papers to topics
weight_thres = .1
topic_papers = gamma %>% 
  mutate(mlr_paper_id = mlr_paper_ids) %>% 
  mutate_at(vars(-mlr_paper_id), funs(ifelse(.>weight_thres, ., NA))) %>% 
  gather(topic_id, weight, -mlr_paper_id, na.rm = TRUE) %>% 
  arrange(topic_id, desc(weight)) %>% 
  left_join(select(papers, mlr_paper_id, title), by="mlr_paper_id") %>% 
  group_by(topic_id) %>% 
  nest(.key = "papers")

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
  out = c(paste("# topic", df$topic_id))
  out = c(out, knitr::kable(df$words[[1]][1:10,]), "\n")
  out = c(out, knitr::kable(df$papers[[1]][1:5,]), "\n")
}

topics %>% 
  group_by(topic_id) %>% 
  split(., .$topic_id) %>% 
  sapply(print_topic) %>% 
  writeLines(file(file.path(data_path, "topics.md")))
