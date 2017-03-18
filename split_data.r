#!/usr/bin/Rscript
require(magrittr)
require(tidyr)
require(dplyr)
require(readr)

set.seed(2017) # for reproducibility

data_path = 'data'
lda_data_path = 'lda_output'

# read users.dat (user bookmarks)
# ------------------------------------------
fn = file.path(data_path, 'users.dat')
cat("reading", fn, "\n")

users = readLines(fn) %>% 
  strsplit(' ') %>% 
  lapply(function(x) data_frame(item = x[-1])) %>% 
  bind_rows(.id = 'user') %>% 
  transmute(user = as.integer(user)-1, # user ids start at 0
            item = as.integer(item))

# items contain the same data but rows are items instead of users
# fn = file.path(data_path, 'items.dat')
# cat("reading", fn, "\n")
# 
# items = readLines(fn) %>% 
#   strsplit(' ') %>% 
#   lapply(function(x) data_frame(user = x[-1])) %>% 
#   bind_rows(.id = 'item') %>% 
#   transmute(item = as.integer(item)-1, # item ids start at 0
#             user = as.integer(user))

# read mult.dat (items word counts)
# ------------------------------------------
fn = file.path(data_path, 'mult.dat')
cat("reading", fn, "\n")

mult = readLines(fn) %>% 
  strsplit(" ")

names(mult) = seq(0, length(mult)-1) # item ids start at 0

mult = mult %>% 
  lapply(function(x) data_frame(word_count = x[-1])) %>% 
  bind_rows(.id = "item") %>% 
  separate(word_count, c("word", "count"), sep = ":") %>% 
  mutate_all(funs(as.integer))

# read final.gamma (items topic proportions)
# ------------------------------------------
fn = file.path(lda_data_path, 'final.gamma')
cat("reading", fn, "\n")

gamma = read_delim(fn, delim = ' ', col_names = FALSE, 
                   col_types = cols(.default = col_double())) %>% 
  mutate(item = seq(0, n()-1)) %>%  # item ids start at 0
  select(item, everything())

# split user bookmarks into library and like
# ------------------------------------------
r = 0.5 # ratio of user library items

cat("splitting with ratio", r, "\n")

# for all users assign each item to a group (library/like)
users = users %>% 
  group_by(user) %>% 
  mutate(group = factor(sample(n()) < r*n(), labels = c('library', 'like'))) %>% 
  ungroup()

users_split = users %>% 
  split(users$group) %>% 
  lapply(function(x) select(x, -one_of("group")))


# save files
# ------------------------------------------
for (nm in names(users_split)) {
  cat("  split:", nm, "\n")
  items = sort(unique(users_split[[nm]][["item"]]))
  
  # filter mult and gamma
  mult_split = mult %>% 
    filter(item %in% items)
  gamma_split = gamma %>% 
    filter(item %in% items)
  
  # reset user and item ids
  users_split[[nm]] = users_split[[nm]] %>% 
    mutate(user = as.integer(droplevels(as.factor(user)))-1,
           item = as.integer(droplevels(as.factor(item)))-1)
  mult_split = mult_split %>%  
    mutate(item = as.integer(droplevels(as.factor(item)))-1)
  gamma_split = gamma_split %>%  
    mutate(item = as.integer(droplevels(as.factor(item)))-1)
  
  # save users file
  fn = file.path(data_path, paste0('users_', nm, '.dat'))
  cat("    writing", fn, "\n")
  
  ucnt = count(users_split[[nm]], user)$n
  
  split(users_split[[nm]]$item, users_split[[nm]]$user) %>% 
    mapply(function(x,y) paste(c(x,y), collapse=" "), ucnt, .) %>% 
    writeLines(fn)
  
  # save items file
  fn = file.path(data_path, paste0('items_', nm, '.dat'))
  cat("    writing", fn, "\n")
  
  icnt = count(users_split[[nm]], item)$n
  
  split(users_split[[nm]]$user, users_split[[nm]]$item) %>% 
    mapply(function(x,y) paste(c(x,y), collapse=" "), icnt, .) %>% 
    writeLines(fn)
  
  # save mult_v file
  # items word counts
  fn = file.path(data_path, paste0('mult_v_', nm, '.dat'))
  cat("    writing", fn, "\n")
  
  wcnt = count(mult_split, item)$n
  
  mult_split %>% 
    unite("word_count", word, count, sep = ":") %>% 
    split(.$item) %>% 
    mapply(function(x,y) paste(c(x, y$word_count), collapse=" "), wcnt, .) %>% 
    writeLines(fn)
  
  # save mult_u file
  # users word counts
  fn = file.path(data_path, paste0('mult_u_', nm, '.dat'))
  cat("    writing", fn, "\n")
  
  mult_user_split = users_split[[nm]] %>% 
    left_join(mult_split, by="item") %>% 
    select(-item) %>% 
    group_by(user, word) %>% 
    summarize(count = sum(count)) %>% 
    ungroup()
  
  uwcnt = count(mult_user_split, user)$n
  
  mult_user_split %>% 
    unite("word_count", word, count, sep = ":") %>% 
    split(.$user) %>% 
    mapply(function(x,y) paste(c(x, y$word_count), collapse=" "), uwcnt, .) %>% 
    writeLines(fn)
  
  # save gamma_v file
  # items topic proportions
  fn = file.path(lda_data_path, paste0('final_', nm, '.gamma_v'))
  cat("    writing", fn, "\n")
  
  gamma_split %>% 
    select(-item) %>% 
    write_delim(fn, delim = ' ', col_names = FALSE)
  
  # save gamma_u file
  # user topic proportions by averaging over their documents
  fn = file.path(lda_data_path, paste0('final_', nm, '.gamma_u'))
  cat("    writing", fn, "\n")
  
  users_split[[nm]] %>% 
    left_join(gamma_split, by = "item") %>% 
    select(-item) %>% 
    group_by(user) %>% 
    summarise_all(funs(mean)) %>% 
    select(-user) %>% 
    write_delim(fn, delim = ' ', col_names = FALSE)
}

