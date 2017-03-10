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
  lapply(function(x) data_frame(item = x[-1])) %>% 
  bind_rows(.id = 'user') %>% 
  transmute(user = as.factor(as.integer(user)-1), # user ids start at 0
            item = as.factor(as.integer(item)))

# items contain the same data but rows are items instead of users

# items = readLines(file.path(data_path, 'items.dat')) %>% 
#   strsplit(' ') %>% 
#   lapply(function(x) data_frame(user = x[-1])) %>% 
#   bind_rows(.id = 'item') %>% 
#   transmute(item = as.factor(as.integer(item)-1), # item ids start at 0
#             user = as.factor(as.integer(user)))

# read mult.dat (items word counts)
# ------------------------------------------
mult = readLines(file.path(data_path, 'mult.dat'))

names(mult) = seq(0, length(mult)-1)

# read final.gamma (items topic proportions)
# ------------------------------------------
gamma = read_delim(file.path(lda_data_path, 'final.gamma'), 
                   delim = ' ', col_names = FALSE) %>% 
  mutate(item = as.factor(seq(0, n()-1))) %>% 
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
  items = sort(unique(users_split[[n]][["item"]]))
  
  # filter gamma
  gamma_split = gamma %>% 
    filter(item %in% items)
  
  # drop item levels
  users_split[[n]] = users_split[[n]] %>% 
    mutate(item = as.factor(as.integer(droplevels(item))-1))
  gamma_split = gamma_split %>%  
    mutate(item = as.factor(as.integer(droplevels(item))-1))
  
  # save mult file
  mult[items] %>% 
    unlist() %>% 
    writeLines(file.path(data_path, paste0('mult_', n, '.dat')))
  
  # save gamma file
  gamma_split %>% 
    select(-item) %>% 
    write_delim(file.path(lda_data_path, paste0('final_', n, '.gamma')), delim = ' ')
  
  # save users file
  ucnt = count(users_split[[n]], user)$n
  
  split(users_split[[n]]$item, users_split[[n]]$user) %>% 
    mapply(function(x,y) paste(c(x,y), collapse=" "), ucnt, .) %>% 
    writeLines(file.path(data_path, paste0('users_', n, '.dat')))
  
  # save items file
  icnt = count(users_split[[n]], item)$n
  
  split(users_split[[n]]$user, users_split[[n]]$item) %>% 
    mapply(function(x,y) paste(c(x,y), collapse=" "), icnt, .) %>% 
    writeLines(file.path(data_path, paste0('items_', n, '.dat')))
}

