source("common.r")
library(tidytext)
library(forcats)
library(tokenizers)
library(topicmodels)
library(pluralize)

# See http://tidytextmining.com

# read papers
papers = fromJSON(file.path(data_path, "papers.json")) %>% 
  as_tibble()

# read texts
#===========================================
txt_files = dir(txt_path, pattern = "\\.txt", full.names = TRUE)

filenames = basename(txt_files) %>%
  tools::file_path_sans_ext()

text_strings = txt_files %>%
  map_chr(~readLines(.) %>% str_c(collapse="\n"))

texts = data_frame(filename = filenames,
                   text = text_strings)

# create stop words
#===========================================
data(stop_words)

latex_stopwords = text_strings %>% 
  str_extract_all("\\\\\\w+") %>% 
  as_vector() %>% 
  unique() %>% 
  str_extract("\\w+") %>% 
  str_to_lower()

my_stopwords = c("known", "setting", "settings", "case", "used", "approach", "approaches", "outperform", "outperforms", 
                 "study", "studies", "better", "important","emph", "small","study", "application", "applications", 
                 "dataset", "datasets", "learn", "learning","parameter", "parameters","mean","means","type",
                 "work","function","functions","sqrt","improve","machine","efficient",
                 "input","world","general","develop","particular","value","significant",
                 "training","framework","simple","provide","number","method","experiment","experiments",
                 "proposed","strongly", "early","models","model","methods","method","propose","paper",
                 "problem","problems","algorithms","algorithm","real","based","function","approach","result",
                 "results","different","performance","extend","demonstrate","standard","datum", "data",
                 "large","existing","using","novel","analysis","introduce","technique","techniques","using",
                 "achieve","synthetic","arxiv")

all_stopwords = stop_words$word %>% 
  c(latex_stopwords) %>%
  c(my_stopwords) %>% 
  unique()

# tokenize and count
#===========================================

text_terms = texts %>% 
  mutate(term = text %>% 
           map(str_extract_all, pattern="\\b[a-zA-Z]\\w{2,}+\\b", simplify=TRUE) %>% # words of 3 characters at least
           map(str_to_lower) %>% 
           map(~discard(., . %in% all_stopwords)) %>% 
           map(singularize) %>% # slow...
           map(~discard(., . %in% all_stopwords)) %>% 
           map(str_c, collapse=" ") %>% 
           tokenize_ngrams(n = 2, n_min = 1)) %>% 
  unnest() %>% 
  count(filename, term, sort = TRUE) %>%
  ungroup() %>% 
  bind_tf_idf(term, filename, n)

text_terms %>%
  top_n(50, tf_idf) %>%
  arrange(desc(tf_idf)) %>%
  mutate(term = as_factor(term) %>% fct_rev()) %>% 
  ggplot(aes(term, tf_idf, fill = filename)) +
  geom_col() + 
  xlab(NULL) +
  coord_flip()

# filter terms
#===========================================
text_terms %>% 
  ggplot(aes(tf_idf)) + 
  geom_histogram()

# min_tfidf = 0.05
min_df = 0.05
max_df = 0.95

text_terms_filtered = text_terms %>% 
  # filter(tf_idf > min_tfidf) %>%
  mutate(df = exp(-idf)) %>%
  filter(df >= min_df, df <= max_df)

terms_filtered = text_terms_filtered %>% 
  distinct(term, idf)

n_obs = sum(text_terms_filtered$n)
n_terms = nrow(terms_filtered)
n_docs = n_distinct(text_terms_filtered$filename)

cat(n_terms, "terms\n")
cat(n_docs, "documents\n")

# Topic modeling: LDA
#===========================================
# convert to DocumentTermMatrix
texts_dtm = text_terms_filtered %>% 
  cast_dtm(filename, term, n)

# grid of parameters for LDA

# k_vec = seq(15,30)
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
  set.seed(2017)
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
    group_by(filename) %>%
    summarise(n=sum(n))

  gamma = texts_lda %>%
    tidy(matrix = "gamma") %>%
    rename(filename=document, topic_id=topic)

  topic_weights = gamma %>%
    left_join(paper_counts, by="filename") %>%
    group_by(topic_id) %>%
    summarize(topic_sum=sum(gamma*n)) %>%
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
    top_n(10, gamma) %>%
    mutate(rk = rank(gamma, ties.method = "first")) %>%
    ungroup() %>%
    left_join(papers %>%
                select(filename, title),
              by="filename") %>%
    ggplot(aes(rk, gamma, fill = factor(topic_id))) +
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
    select(filename, subject_areas) %>%
    filter(subject_areas %>% map_lgl(is.data.frame)) %>%
    mutate(primary_subject_area = map(subject_areas, slice, 1)) %>%
    unnest(primary_subject_area)

  gamma_subjects = gamma %>%
    left_join(papers_subject_areas, by = "filename") %>%
    left_join(paper_counts, by="filename") %>%
    group_by(topic_id, subject_area_1, subject_area_2) %>%
    summarise(gamma = sum(gamma*n)) %>%
    group_by(subject_area_1, subject_area_2) %>%
    mutate(gamma = gamma/sum(gamma))

  gamma_subjects %>%
    group_by(topic_id) %>%
    top_n(10, gamma) %>%
    mutate(rk = rank(gamma, ties.method = "first")) %>%
    ungroup() %>%
    ggplot(aes(rk, gamma, fill = factor(topic_id))) +
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
    select(filename, topic_id, weight = gamma) %>%
    group_by(filename) %>%
    arrange(desc(weight)) %>%
    nest(.key = "topics")

  topic_papers_nested = gamma %>%
    select(topic_id, filename, weight = gamma) %>%
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
    select(topic_id, subject_area_1, subject_area_2, weight = gamma) %>%
    group_by(topic_id) %>%
    arrange(desc(weight)) %>%
    nest(.key = "subject_areas")

  # topics term proportions
  # =======================
  topic_words_nested = beta %>%
    select(topic_id, term, weight=beta) %>%
    group_by(topic_id) %>%
    arrange(desc(weight)) %>%
    nest(.key = "terms")

  topics = topic_weights %>%
    arrange(desc(topic_weight)) %>%
    left_join(topic_words_nested, by="topic_id") %>%
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
    # mutate(labels = map(terms, ~.$term[1])) %>%  ############### TEMP #####
  mutate(labels = map(subject_areas, ~.$subject_area_1[1:2])) %>%  ############### TEMP #####
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

    close(fc)
  }
  
}

# compute information criterions
# -----------------------------
lda_params = lda_params %>%
  mutate(p = k*(n_terms-1) + n_docs*(k-1)) %>% # nb of free parameters
  mutate(aic = -2*log_like+2*p+2*p*(p+1)/(n_obs-p-1)) %>%
  mutate(bic = -2*log_like+p*log(n_obs)) %>%
  mutate(log_pp = (-log_like + p)/n_obs) # log penalized perplexity
# NOTE: these criterions might be wrong if the loglike returned by LDA is up to a constant

lda_params %>%
  filter(alpha<1) %>% 
  ggplot(aes(k, aic)) +
  geom_point()

ggsave(file.path(data_path, "lda_aic.png"), width=8, height=6)
