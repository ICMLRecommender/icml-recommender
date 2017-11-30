#!/usr/bin/Rscript --slave

source("common.r")
library(tidytext)
library(forcats)
library(tokenizers)
library(pluralize)

# See http://tidytextmining.com

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

# custom stopwords
my_stopwords = readLines(stopwords_path) %>% 
  str_trim() %>% 
  str_to_lower()

# english stopwords
data(stop_words, package = "tidytext")

# latex stopwords
latex_stopwords = text_strings %>% 
  str_extract_all("\\\\\\w+") %>% 
  as_vector() %>% 
  unique() %>% 
  str_extract("\\w+") %>% 
  str_to_lower()

all_stopwords = stop_words$word %>% 
  c(latex_stopwords) %>%
  c(my_stopwords) %>% 
  unique()

# tokenize and count
#===========================================

text_terms = texts %>% 
  mutate(term = text %>% 
           map(str_extract_all, pattern="\\b[a-zA-Z]\\w{2,}+\\b", simplify=TRUE) %>% # words of 3 characters at least
           map(str_to_lower) %>% # to lower case
           map(~discard(., . %in% all_stopwords)) %>% 
           map(singularize) %>% # plurals to singular. SLOW...
           map(~discard(., . %in% all_stopwords)) %>% # remove stopwords
           map(str_c, collapse=" ") %>% 
           tokenize_ngrams(n = 2, n_min = 1)) %>% # uni and bigrams
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
min_df = 0.05 # min document frequency
max_df = 0.95 # max document frequency

text_terms_filtered = text_terms %>% 
  # filter(tf_idf > min_tfidf) %>%
  mutate(df = exp(-idf)) %>%
  filter(df >= min_df, df <= max_df)

n_obs = sum(text_terms_filtered$n)
n_terms = n_distinct(text_terms_filtered$term)
n_docs = n_distinct(text_terms_filtered$filename)

cat(n_terms, "terms\n")
cat(n_docs, "documents\n")


# write dat files
#============================================

text_terms_filtered = text_terms_filtered %>% 
  mutate(filename = as_factor(filename),
         term = as_factor(term))

# files file
filenames = levels(text_terms_filtered$filename)

filenames %>% 
  writeLines(files_path)

# vocab file
vocab = levels(text_terms_filtered$term)

vocab %>% 
  writeLines(vocab_path)

# mult file
text_terms_filtered %>% 
  mutate(file_id = as.integer(filename)-1,
         term_id = as.integer(term)-1) %>% 
  arrange(file_id, term_id) %>% 
  group_by(file_id) %>% 
  summarise(string = paste(n(), paste(term_id, n, sep=":", collapse=" "))) %>% 
  pull(string) %>% 
  writeLines(mult_path)
