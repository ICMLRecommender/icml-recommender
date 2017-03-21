# Split NIPS data into train and test according to fold 
# Author: Valerio Perrone
# March 2017


fold <- 2  #(NOTE!)

read.user <- function(filename, offset=1)
{
  one <- scan(filename, what = "", sep = "\n", quiet=T)
  two <- strsplit(one, " ", fixed = TRUE)
  lapply(two, function(x) (offset+as.vector(as.integer(x[-1]))))
}


#read.user('nips_user.dat',offset=1)


write.user <- function(user, filename, offset=-1)
{
  f <- file(filename, "w")
  for (u in user)
  {
    u <- u + offset
    line <- paste(length(u), paste(u, collapse=" "))
    write(x=line, file=f, append=T)
  }
  close(f)
}

## construct item matrix from user matrix
item.mat.from.user <- function(user, num.items)
{
  item <- rep.int(list(NULL), num.items) 
  
  # get number of items from users
  num.users <- length(user)
  for (i in 1:num.users)
  {
    item.ids <- user[[i]]
    for (j in item.ids) item[[j]] <- c(item[[j]], i)
  }
  
  for (j in 1:num.items)
  {
    if (!is.null(item[[j]])) item[[j]] <- sort(item[[j]])
  }
  
  item
}

read.user.and.write.item <- function(user.path, item.path, num.items) {
  user <- read.user(user.path)
  item <- item.mat.from.user(user, num.items)
  write.user(user=item, filename=item.path)
}

user.mat.from.item <- function(item, num.users)
{
  user <- rep.int(list(NULL), num.users)
  num.items <- length(item)
  for (i in 1:num.items)
  {
    user.ids <- item[[i]]
    for (j in user.ids) user[[j]] <- c(user[[j]], i)
  }
  for (j in num.users)
  {
    if (!is.null(user[[j]])) user[[j]] <- sort(user[[j]])
  }
  user
}



ofm.split.data <- function(path, fold, outpath) {
  user <- read.user(path)
  
  num.items <- max(sapply(user, FUN=max)) 
  splits <- sample(1:num.items)
  item.per.fold <- num.items/fold
  stats <- rep(0, fold) 
  for (i in 1:fold)
  {
    cat(sprintf("fold %d\n", i))
    start <- (i-1) * item.per.fold + 1
    end <- i * item.per.fold 
    test.idx <- sort(splits[start:end])
    train.idx <- (1:num.items)[-test.idx]
    train <- list()
    test <- list()
    for (u in user)
    {
      u.train <- intersect(u, train.idx) 
      u.test <- intersect(u, test.idx) 
      if (length(u.train) == 0) stop("useless\n")
      train <- c(train, list(u.train))
      test <- c(test, list(u.test))
    }
    write.user(train, filename=sprintf("%s/ofm-train-%d-users.dat", outpath, i))
    write.user(test, filename=sprintf("%s/ofm-test-%d-users.dat", outpath, i))
    write(test.idx, file=sprintf("%s/heldout-set-%d.dat", outpath, i))
  }
}


random.divide <- function(x, fold)
{
  n <- length(x)
  x <- sample(x)
  k <- as.integer(n/fold)
  y <- list()
  for (i in 1:fold)
  {
    s <- (i-1) * k + 1
    e <- i * k
    if (i == fold) e <- n
    y <- c(y, list(sort(x[s:e])))
  }
  y
}

create.cf.splits <- function(path, fold=5) {
  all.users <- read.user(sprintf("%s/nips_user.dat", path))
  num.users <- length(all.users)
  splits <- rep(list(NULL), num.users)
  set.seed(4095995)
  num.items <- max(sapply(all.users, max))
  all.items <- 1:num.items
  cat(sprintf("number of items: %d\n", num.items))
  for (i in 1:num.users)
  {
    user.items <- all.users[[i]]
    unrated.items <- all.items[-user.items]
    splits[[i]] <- random.divide(unrated.items, fold)
  }
  
  for (i in 1:fold)
  {
    user.test <- read.user(sprintf("%s/cf-test-%d-users.dat", path, i))
    for (j in 1:num.users)
    {
      rated.items.test <- user.test[[j]]
      unrated.items <- splits[[j]][[i]]
      splits[[j]][[i]] <- sort(c(unrated.items, rated.items.test))
    }
  }
  save(splits, file=sprintf("%s/splits.cf.dat", path))
}

cf.split.data <- function(path, fold, outpath)
{
  user <- read.user(path)
  num.users <- length(user)
  
  num.items <- max(sapply(user, FUN=max)) 
  
  items <- item.mat.from.user(user, num.items)
  train <- rep(list(list()), fold)
  test <- rep(list(list()), fold)
  j <- 0
  for (user.ids in items)
  {
    j <- j + 1
    if (j %% 1000 == 0) cat(sprintf("doc %d\n", j))
    
    n <- length(user.ids) 
    if (n >= fold) 
    {
      idx <- 1:n
      user.ids.folds <- random.divide(idx, fold)
      for (i in 1:fold)
      {
        test.idx <- user.ids.folds[[i]]
        train.idx <- idx[-test.idx]
        train[[i]] <- c(train[[i]], list(user.ids[train.idx]))
        test[[i]] <- c(test[[i]], list(user.ids[test.idx]))
      }
    }
    else
    {
      for (i in 1:fold)
      {
        train[[i]] <- c(train[[i]], list(user.ids))
        test[[i]] <- c(test[[i]], list(NULL))
      }
    }
  }
  
  for (i in 1:fold)
  {
    item.train <- train[[i]] 
    user.train <- user.mat.from.item(item.train, num.users)
    x <- min(sapply(user.train, FUN=length))
    if (x == 0) cat("some users contains 0 items, run again\n")
    write.user(user.train, filename=sprintf("%s/cf-train-%d-users.dat", outpath, i))
    item.test <- test[[i]] 
    user.test <- user.mat.from.item(item.test, num.users)
    write.user(user.test, filename=sprintf("%s/cf-test-%d-users.dat", outpath, i))
  }
}



#RUN in R
ofm.split.data("/home/valerio/processed_nips/nips_user.dat",fold,"/home/valerio/processed_nips/")
cf.split.data("/home/valerio/processed_nips/nips_user.dat",fold,"/home/valerio/processed_nips/")
create.cf.splits("/home/valerio/processed_nips", fold=fold) # ---> saves splits data in path: save(splits, file=sprintf(“%s/#splits.cf.dat", path))
