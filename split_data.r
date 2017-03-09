require(tidyr)
require(dplyr)
require(readr)

set.seed(2017) # for reproducibility

data_path = 'data'
lda_data_path = 'lda_output'

# read users.dat (user bookmarks)
# ------------------------------------------
users = readLines(file.path(data_path, 'users.dat')) %>% 
  strsplit(' ') %>% 
  lapply(tbl_df) %>% 
  bind_rows(.id = 'user') %>% 
  transmute(user = as.factor(as.integer(user)), 
            item = as.factor(as.integer(value)))

# read mult.dat (items word counts)
# ------------------------------------------
mult = readLines(file.path(data_path, 'mult.dat')) %>% 
  strsplit(' ')

items = sapply(mult, function(x) x[[1]])
names(mult) = items

mult = mult %>% 
  lapply(function(x) data_frame(word_count = x[-1])) %>% 
  bind_rows(.id = "item") %>% 
  mutate(item = as.factor(as.integer(item)))

# read final.gamma (items topic proportions)
# ------------------------------------------
gamma = read_delim(file.path(lda_data_path, 'final.gamma'), 
                   delim = ' ', col_names = FALSE) %>% 
  mutate(item = as.factor(1:n())) %>% 
  select(item, everything())

# split user bookmarks into library and like
# ------------------------------------------
r = 0.5 # ratio of user library items

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
for (n in names(users_split)) {
  # filter mult
  mult_split = mult %>% 
    filter(item %in% unique(users_split[[n]][["item"]]))
  
  # filter gamma
  gamma_split = gamma %>% 
    filter(item %in% unique(users_split[[n]][["item"]]))
  
  # drop item levels
  users_split[[n]] = users_split[[n]] %>% 
    mutate(item = as.factor(as.integer(droplevels(item))))
  gamma_split = gamma_split %>%  
    mutate(item = as.factor(as.integer(droplevels(item))))
  mult_split = mult_split %>%  
    mutate(item = as.factor(as.integer(droplevels(item))))
  
  # save mult file
  mult_split = split(mult_split$word_count, mult_split$item)
  
  mapply(function(x,y) paste(c(x,y), collapse=" "), 
         names(mult_split), mult_split) %>% 
    writeLines(file.path(data_path, paste0('mult_', n, '.dat')))
  
  # save gamma file
  gamma_split %>% 
    select(-item) %>% 
    write_delim(file.path(lda_data_path, paste0('final_', n, '.gamma')), delim = ' ')
  
  # save users file
  split(users_split[[n]]$item, users_split[[n]]$user) %>% 
    lapply(function(x) paste(x, collapse=" ")) %>% 
    unlist() %>% 
    writeLines(file.path(data_path, paste0('users_', n, '.dat')))
  
  # save items file
  split(users_split[[n]]$user, users_split[[n]]$item) %>% 
    lapply(function(x) paste(x, collapse=" ")) %>% 
    unlist() %>% 
    writeLines(file.path(data_path, paste0('items_', n, '.dat')))
}

