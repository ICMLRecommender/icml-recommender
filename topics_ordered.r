require(dplyr)
require(tidyr)
require(readr)
require(jsonlite)
require(knitr)

data_path = "data/icml2016"


args <- commandArgs(trailingOnly = TRUE)
if (length(args)<1) {
    stop("Argument required: directory name with results", call.=FALSE)
}
res_dir <- args[1]





# topics
words = readLines(file.path(data_path, "vocab.dat"))

beta = read_delim(file.path(data_path, sprintf("%s/final.beta", res_dir)),
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
  mutate(top_words = sapply(words, function(x) paste(x$word[1:3], collapse=" ")))

# papers topic proportions
# =============================
file_keys = readLines(file.path(data_path, "papers_txt/files.dat")) %>% 
  basename() %>% tools::file_path_sans_ext()

gamma = read_delim(file.path(data_path, sprintf("%s/final.gamma", res_dir)),
                          delim=" ", col_names = as.character(seq_len(n_topics))) %>% 
  sweep(., 1, rowSums(.), "/")


av_gamma = apply(gamma,2,mean)
ordered_topics = order(av_gamma,decreasing=TRUE)

papers = fromJSON(file(file.path(data_path, "papers.json"))) %>% tbl_df() 

# add topic proportions to papers
weight_thres = .1
paper_topics = gamma %>% 
  mutate(key = file_keys) %>% 
  mutate_at(vars(-key), funs(ifelse(.>weight_thres, ., NA))) %>% 
  gather(topic_id, weight, -key, na.rm = TRUE) %>% 
  arrange(key, desc(weight)) %>% 
  group_by(key) %>% 
  nest(.key = "topics")

papers = papers %>% 
  left_join(paper_topics, by = "key")

# add top papers to topics
weight_thres = .1
topic_papers = gamma %>% 
  mutate(key = file_keys) %>% 
  mutate_at(vars(-key), funs(ifelse(.>weight_thres, ., NA))) %>% 
  gather(topic_id, weight, -key, na.rm = TRUE) %>% 
  mutate(topic_id = as.integer(topic_id)) %>% 
  arrange(topic_id, desc(weight)) %>% 
  left_join(select(papers, key, title), by="key") %>% 
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
  write(file.path(data_path, "papers.json"))


# write topics.txt
#===================

print_topic = function(df) {
  out = c(paste("# topic", df$topic_id))
  out = c(out, knitr::kable(df$words[[1]][1:10,]))
  out = c(out, "\n")
  out = c(out, knitr::kable(df$papers[[1]][1:5,]))
  out = c(out, "\n")
}

unordered = topics %>%
  group_by(topic_id) %>%
  split(., .$topic_id)

unordered[ordered_topics] %>%
  sapply(print_topic) %>%
  writeLines(file(file.path(data_path, sprintf("%s/topics.md", res_dir))))
