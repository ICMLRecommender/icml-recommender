#!/usr/bin/Rscript --slave

source("common.r")
library(tidytext)
library(forcats)
library(tokenizers)
library(topicmodels)
library(pluralize)

# See http://tidytextmining.com

# read text terms
filenames = readLines(files_path) %>% 
  as_factor()

vocab = readLines(vocab_path) %>% 
  as_factor()

text_terms_filtered = readLines(mult_path) %>% 
  str_extract_all("\\d+:\\d+") %>% 
  map(str_split, ":", simplify=TRUE) %>% 
  map(set_colnames, c("term_id", "n")) %>% 
  map(as_tibble) %>% 
  bind_rows(.id = "file_id") %>% 
  transmute(filename = filenames[as.integer(file_id)],
            term = vocab[as.integer(term_id)+1],
            n = as.integer(n))

# read papers
papers = fromJSON(file.path(data_path, "papers.json")) %>% 
  as_tibble()

#===============================================================================
# Topic modeling: LDA
#===============================================================================

# convert to DocumentTermMatrix

texts_dtm = text_terms_filtered  %>% 
  cast_dtm(filename, term, n)

# grid of parameters for LDA

# k_vec = c(20, 25, 30)
# alpha_vec = c(0.05, 0.1, 0.2, 0.5)

k_vec = lda_n_topics
alpha_vec = lda_alpha

lda_params = tidyr::crossing(k = k_vec, alpha = alpha_vec) %>% 
  mutate(log_like = NA)

# loop over the grid
for (i in seq_len(nrow(lda_params))) {
  k = lda_params$k[i]
  alpha = lda_params$alpha[i]
  
  cat("running lda: k =", k, "\n")
  
  # LDA settings
  lda_ctrl = list(alpha = alpha,
                  estimate.alpha = FALSE, 
                  verbose = 1, 
                  nstart=1,
                  seed = 4357,
                  var = list(iter.max=500, tol=1e-6),
                  em = list(iter.max=1000, tol=1e-4),
                  keep = 1)

  
  # run LDA
  texts_lda = texts_dtm %>% 
    LDA(k = k, method = "VEM", control = lda_ctrl)
  
  lda_params$alpha[i] = texts_lda@alpha # in case alpha is estimated
  lda_params$log_like[i] = last(texts_lda@logLiks)
  
  lda_suffix = paste0("_k", lda_params$k[i], "_alpha", format(lda_params$alpha[i], digits = 2))
  
  # plot log likelihood along iterations
  data_frame(iter = seq_len(length(texts_lda@logLiks)),
             log_like=texts_lda@logLiks) %>%
    ggplot(aes(iter, log_like)) +
    geom_point()

  # get topics word distributions
  beta = texts_lda %>%
    tidy(matrix = "beta") %>%
    rename(topic_id=topic)

  # get documents topic distributions
  paper_counts = text_terms_filtered %>%
    mutate(filename = as.character(filename)) %>% 
    group_by(filename) %>%
    summarise(n=sum(n))

  gamma = texts_lda %>%
    tidy(matrix = "gamma") %>%
    rename(filename=document, topic_id=topic) %>% 
    left_join(paper_counts, by="filename") %>%
    mutate(gamma=n*gamma) %>% # gamma unnormalized
    group_by(topic_id) %>%
    mutate(gamma_norm=gamma/sum(gamma)) %>% # gamma normalized by topic
    ungroup()

  topic_weights = gamma %>%
    group_by(topic_id) %>%
    summarize(topic_sum=sum(gamma)) %>%
    ungroup() %>%
    mutate(topic_weight=topic_sum/sum(topic_sum))
  
  # get word assignments
  assignments = texts_lda %>%
    augment(data = texts_dtm) %>%
    rename(filename=document, topic_id=.topic)

  # plot topics terms
  #===========================================
  topic_labeller = function(topic_id) {
    ind = match(topic_id, topic_weights$topic_id)
    str_c("topic ", topic_id, " [", format(100*topic_weights$topic_weight[ind], digits = 2), "%]")
  }

  beta %>%
    group_by(topic_id) %>%
    top_n(10, beta) %>%
    mutate(rk = rank(beta, ties.method = "first")) %>%
    ungroup() %>%
    ggplot(aes(rk, beta, fill = factor(topic_id))) +
    geom_col(show.legend = FALSE, alpha=.4) +
    geom_text(aes(rk, y=0, label=term), hjust=0) +
    facet_wrap(~ topic_id, scales = "free", labeller = as_labeller(topic_labeller)) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    xlab(NULL) + ylab(NULL)

  ggsave(file.path(data_path, paste0("topics_terms", lda_suffix, ".png")), height=9, width=16)

  # plot topics papers
  #===========================================

  gamma %>%
    group_by(topic_id) %>%
    top_n(10, gamma_norm) %>%
    mutate(rk = rank(gamma_norm, ties.method = "first")) %>%
    ungroup() %>%
    left_join(papers %>%
                select(filename, title),
              by="filename") %>%
    ggplot(aes(rk, gamma_norm, fill = factor(topic_id))) +
    geom_col(show.legend = FALSE, alpha=.3) +
    geom_text(aes(rk, y=0, label=title), hjust=0) +
    facet_wrap(~ topic_id, scales = "free", labeller = as_labeller(topic_labeller)) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    xlab(NULL) + ylab(NULL)

  ggsave(file.path(data_path, paste0("topics_papers", lda_suffix, ".png")), height=9, width=16)

  # subject topic proportions
  # =============================

  papers_subject_areas = papers %>%
    select(filename, subject_area_1, subject_area_2)

  gamma_subjects = gamma %>%
    left_join(papers_subject_areas, by = "filename") %>%
    group_by(topic_id, subject_area_1, subject_area_2) %>%
    summarise(gamma = sum(gamma)) %>%
    group_by(topic_id) %>%
    mutate(gamma_norm = gamma/sum(gamma)) %>% 
    ungroup()

  gamma_subjects %>%
    group_by(topic_id) %>%
    top_n(10, gamma_norm) %>%
    mutate(rk = rank(gamma_norm, ties.method = "first")) %>%
    ungroup() %>%
    ggplot(aes(rk, gamma_norm, fill = factor(topic_id))) +
    geom_col(show.legend = FALSE, alpha=.3) +
    geom_text(aes(rk, y=0, label=str_c(subject_area_1, "/" ,subject_area_2)), hjust=0) +
    facet_wrap(~ topic_id, scales = "free", labeller = as_labeller(topic_labeller)) +
    coord_flip() +
    theme_minimal() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank()) +
    xlab(NULL) + ylab(NULL)

  ggsave(file.path(data_path, paste0("topics_subjects", lda_suffix, ".png")), height=9, width=16)

  # plot graph of terms based on topic co-occurences
  #=================================================
  library(igraph)
  beta_filtered = beta %>%
    filter(beta>0.03)

  terms_graph = beta_filtered %>%
    left_join(beta_filtered,
              by="topic_id") %>%
    filter(term.x < term.y) %>%
    group_by(term.x, term.y) %>%
    summarise(weight = sum(beta.x*beta.y)) %>%
    ungroup() %>%
    igraph::graph_from_data_frame(directed=FALSE)

  library(ggraph)
  set.seed(2017)
  terms_graph %>%
    ggraph(layout="fr") +
    geom_edge_link(aes(edge_alpha = weight), show.legend = FALSE) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()

  ggsave(file.path(data_path, paste0("terms_graph", lda_suffix, ".png")), height=9, width=16)

  # papers topic proportions
  # =============================
  paper_topics_nested = gamma %>%
    select(filename, topic_id, weight = gamma_norm) %>%
    group_by(filename) %>%
    arrange(desc(weight)) %>%
    nest(.key = "topics")

  topic_papers_nested = gamma %>%
    select(topic_id, filename, weight = gamma_norm) %>%
    left_join(papers %>%
                select(paper_id, filename),
              by="filename") %>%
    select(-filename) %>%
    group_by(topic_id) %>%
    arrange(desc(weight)) %>%
    nest(.key = "papers")


  # subject topic proportions
  # =============================

  topic_subjects_nested = gamma_subjects %>%
    select(topic_id, subject_area_1, subject_area_2, weight = gamma_norm) %>%
    group_by(topic_id) %>%
    arrange(desc(weight)) %>%
    nest(.key = "subject_areas")

  # topics term proportions
  # =======================
  topic_terms_nested = beta %>%
    select(topic_id, term, weight=beta) %>%
    group_by(topic_id) %>%
    arrange(desc(weight)) %>%
    nest(.key = "terms")

  # topic clusters
  #================

  topic_labels_file = file.path(raw_path, "topic_labels.csv")

  download.file(topic_labels_url, topic_labels_file)

  topic_labels = read_delim(topic_labels_file, delim = ",", trim_ws = TRUE) %>%
    select(topic_id, labels) %>%
    mutate(labels = str_split(labels, "/") %>%
             map(str_trim))
  
  topic_topic_clusters = topic_weights %>%
    # left_join(topic_terms_nested, by="topic_id") %>%
    # mutate(labels = map(terms, ~.$term[1])) %>%  # use first word
    # left_join(topic_subjects_nested, by="topic_id") %>%
    # mutate(labels = map(subject_areas, ~.$subject_area_1[1:2])) %>% # use first subject_area_1
    left_join(topic_labels, by = "topic_id") %>% # use handmade topic labels
    mutate(weight = 1/map_int(labels, length)) %>% 
    unnest(labels, .drop = FALSE) %>%
    rename(label = labels) %>%
    filter(!is.na(label))
  
  topic_cluster_weights = topic_topic_clusters %>%
    group_by(label) %>%
    summarize(topic_cluster_sum=sum(weight*topic_sum),
              topics = list(data_frame(topic_id = topic_id,
                                  weight = weight/sum(weight)))) %>% 
    mutate(topic_cluster_weight=topic_cluster_sum/sum(topic_cluster_sum)) %>% 
    arrange(desc(topic_cluster_weight)) %>% 
    mutate(topic_cluster_id = seq_len(n()))
  
  topic_topic_clusters = topic_topic_clusters %>% 
    left_join(topic_cluster_weights %>% 
                select(label, topic_cluster_id), by = "label") %>% 
    select(topic_cluster_id, everything())
  
  topic_clusters_nested = topic_topic_clusters %>% 
    select(topic_id, topic_cluster_id, weight) %>% 
    group_by(topic_id) %>% 
    nest(.key = "topic_clusters")
  
  # join to topics
  topics = topic_weights %>%
    arrange(desc(topic_weight)) %>%
    left_join(topic_terms_nested, by="topic_id") %>%
    left_join(topic_papers_nested, by="topic_id") %>%
    left_join(topic_subjects_nested, by="topic_id") %>%
    left_join(topic_clusters_nested, by="topic_id")
  
  
  # Analyse topic clusters
  # ===========================
  
  # topic clusters terms
  topic_cluster_terms_nested = beta %>% 
    left_join(topic_topic_clusters, by = "topic_id") %>% 
    group_by(label, term) %>% 
    summarise(beta = sum(beta*topic_sum*weight)) %>% 
    group_by(label) %>% 
    mutate(beta = beta/sum(beta)) %>% 
    ungroup() %>% 
    left_join(topic_cluster_weights %>% 
                select(label, topic_cluster_id), by = "label") %>% 
    select(topic_cluster_id, term, weight = beta) %>% 
    group_by(topic_cluster_id) %>% 
    nest(.key = "terms")
  
  # topic clusters papers
  topic_cluster_papers_nested = gamma %>% 
    left_join(topic_topic_clusters, by = "topic_id") %>% 
    left_join(papers %>%
                select(paper_id, filename),
              by="filename") %>%
    select(-filename) %>% 
    group_by(label, paper_id) %>% 
    summarise(gamma = sum(gamma*weight)) %>% 
    group_by(label) %>% 
    mutate(gamma_norm = gamma/sum(gamma)) %>% 
    ungroup() %>% 
    left_join(topic_cluster_weights %>% 
                select(label, topic_cluster_id), by = "label") %>% 
    select(topic_cluster_id, paper_id, weight = gamma_norm) %>% 
    group_by(topic_cluster_id) %>% 
    nest(.key = "papers")
  
  # topic clusters subjects
  topic_cluster_subjects_nested = gamma_subjects %>% 
    left_join(topic_topic_clusters, by = "topic_id") %>% 
    group_by(label, subject_area_1, subject_area_2) %>% 
    summarise(gamma = sum(gamma*weight)) %>% 
    group_by(label) %>% 
    mutate(gamma_norm = gamma/sum(gamma)) %>% 
    ungroup() %>% 
    left_join(topic_cluster_weights %>% 
                select(label, topic_cluster_id), by = "label") %>% 
    select(topic_cluster_id, subject_area_1, subject_area_2, weight = gamma_norm) %>% 
    group_by(topic_cluster_id) %>% 
    nest(.key = "subject_areas")
  
  # join 
  topic_clusters = topic_cluster_weights %>%
    left_join(topic_cluster_terms_nested, by="topic_cluster_id") %>%
    left_join(topic_cluster_papers_nested, by="topic_cluster_id") %>%
    left_join(topic_cluster_subjects_nested, by="topic_cluster_id")
  
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
      subject_areas = df$subject_areas[[1]] %>%
        arrange(desc(weight)) %>%
        select(weight, subject_area_1, subject_area_2) %>%
        slice(seq_len(n))
      out = c(out, knitr::kable(subject_areas), "\n")
    }

    paper_titles = papers %>%
      select(paper_id, title)

    fc = file(file.path(data_path, paste0("topics", lda_suffix, ".md")))

    topics %>%
      group_by(topic_id) %>%
      do(mutate(., papers = map(papers, ~left_join(., paper_titles, by = "paper_id")))) %>%
      split(seq_len(nrow(.))) %>%
      sapply(print_topic) %>%
      writeLines(fc)
    
    print_topic_cluster = function(df, n=10) {
      out = c(str_c("# [", format(df$topic_cluster_weight*100, digit=3), "%] topic_cluster ", df$topic_cluster_id, ": ", df$label))
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
      subject_areas = df$subject_areas[[1]] %>%
        arrange(desc(weight)) %>%
        select(weight, subject_area_1, subject_area_2) %>%
        slice(seq_len(n))
      out = c(out, knitr::kable(subject_areas), "\n")
    }
    
    fc = file(file.path(data_path, paste0("topic_clusters", lda_suffix, ".md")))
    
    topic_clusters %>%
      group_by(topic_cluster_id) %>%
      do(mutate(., papers = map(papers, ~left_join(., paper_titles, by = "paper_id")))) %>%
      split(seq_len(nrow(.))) %>%
      sapply(print_topic_cluster) %>%
      writeLines(fc)

    close(fc)
  }
  
}

# write theta_v.dat
# -----------------------------

theta_v_path = file.path(data_path, "theta_v.dat")
  
gamma %>% 
  select(filename, topic_id, gamma) %>% 
  spread(topic_id, gamma) %>% 
  arrange(factor(filename, levels = filenames)) %>% 
  select(-filename) %>% 
  write_delim(theta_v_path, delim = " ", col_names = FALSE)

# compute information criterions
# -----------------------------
lda_params = lda_params %>%
  mutate(p = k*(n_terms-1) + n_docs*(k-1)) %>% # nb of free parameters
  mutate(aic = -2*log_like+2*p+2*p*(p+1)/(n_obs-p-1)) %>%
  mutate(bic = -2*log_like+p*log(n_obs)) %>%
  mutate(log_pp = (-log_like + p)/n_obs) # log penalized perplexityT
# NOTE: these criterions might be wrong if the loglike returned by LDA is up to a constant

lda_params %>%
  filter(alpha<1) %>% 
  ggplot(aes(k, aic)) +
  geom_point()

ggsave(file.path(data_path, "lda_aic.png"), width=8, height=6)
