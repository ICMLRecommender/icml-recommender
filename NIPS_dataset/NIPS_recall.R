# Compute recall on NIPS data
# Author: Valerio Perrone
# March 2017

fold <- 2
NUM_TOPICS <- 10


#SOURCE in R #Computes scores and get a score.dat in each cv folder.
scoring <- function(path, num.topics) { ### computes U X V
  u.path <- sprintf("%s/final-U.dat", path)
  u <- scan(u.path)
  u <- matrix(u, nrow=num.topics)
  
  v.path <- sprintf("%s/final-V.dat", path)
  v <- scan(v.path)
  v <- matrix(v, nrow=num.topics)
  score <- t(u) %*% v
  score.path <- sprintf("%s/score.dat", path)
  cat(sprintf("computing score and saving to %s ...\n", score.path))
  save(score, file=score.path, compress=T)
}

scoring.all <- function(root.path, num.topics) {
  paths <- list.files(root.path, pattern="cv-")
  for (path in paths) {
    result.path <- paste(root.path, path, sep="/")
    cat(sprintf("scoring %s ...\n", result.path))
    scoring(result.path, num.topics)
  }
}



dyn.load("utils.so")

#install.packages('plyr')
library(plyr)


breaks <- seq(20, 200, 20) # it is the number of articles to recommend to compute recall. I have 1251 articles, so recommend top breaks articles (up to 200) to users

read.user <- function(filename, offset=1) {
  one <- scan(filename, what = "", sep = "\n", quiet=T)
  two <- strsplit(one, " ", fixed = TRUE)
  lapply(two, function(x) (offset + as.vector(as.integer(x[-1]))))
}


summary.recall <- function(root.path) { ##just puts together all the computed recalls
  users <- read.user(sprintf("%s/nips_user.dat", root.path))
  nbibs <- data.frame(user.id=1:length(users), total=sapply(users, length))
  recall <- NULL
  
  #paths <- sort(list.files(root.path, pattern="cv-cf-[1-5]"))
  #for (i in seq(fold)) {
  #  path <- paths[i]
  #  recall.tmp <- read.csv(sprintf("%s/%s/recall-user.dat", root.path, path))
  #  recall <- rbind(recall, data.frame(method="cf-in-matrix", recall.tmp))
  #}
  
  #ctr + cf
  #paths <- sort(list.files(root.path, pattern="cv-ctr-[1-5]-cf"))
  #for (i in seq(fold)) {
  #  path <- paths[i]
  #  recall.tmp <- read.csv(sprintf("%s/%s/recall-user.dat", root.path, path))
  #  recall <- rbind(recall, data.frame(method="ctr-in-matrix", recall.tmp))
  #}
  
  #ctr + cf
  paths <- sort(list.files(root.path, pattern="cv-ctr-[1-5]"))
  for (i in seq(fold)) {
    path <- paths[i]
    recall.tmp <- read.csv(sprintf("%s/%s/recall-user.dat", root.path, path))
    recall <- rbind(recall, data.frame(method="ctr-in-matrix", recall.tmp))
  }
  
  #ctr + ofm
  #paths <- sort(list.files(root.path, pattern="cv-ctr-[1-5]-ofm"))
  #for (i in seq(fold)) {
  #  path <- paths[i]
  #  recall.tmp <- read.csv(sprintf("%s/%s/recall-user.dat", root.path, path))
  #  recall <- rbind(recall, data.frame(method="ctr-out-matrix", recall.tmp))
  #}
  recall <- merge(recall, nbibs)
  recall$recall <- fold*recall$recall/recall$total
  recall <- ddply(recall, .(method, N), function(df) mean(df$recall))
  names(recall) <- c("method", "N", "recall")
  recall
}



compute.recall.all <- function(root.path) {
  cat(sprintf("reading from %s\n", root.path))
  splits.cf.path <- sprintf("%s/splits.cf.dat", root.path)
  cat(sprintf("loading %s ...\n", splits.cf.path)) 
  load(splits.cf.path)
  
  #cf
  #paths <- sort(list.files(root.path, pattern="cv-cf-[1-5]"))
  #cf.compute.recall(root.path, paths, splits)
  
  #ctr + cf
  #paths <- sort(list.files(root.path, pattern="cv-ctr-[1-5]-cf"))
  #cf.compute.recall(root.path, paths, splits)
  
  #ctr + ofm
  #paths <- sort(list.files(root.path, pattern="cv-ctr-[1-5]-ofm"))
  #ofm.compute.recall(root.path, paths)
  
  #only ctr (on in matrix predictions I guess) !!!
  paths <- sort(list.files(root.path, pattern="cv-ctr-[1-5]"))
  cf.compute.recall(root.path, paths, splits)
}


cf.compute.recall <- function(root.path, paths, splits, top=200) {
  for (i in seq(fold)) {
    path <- paths[i] 
    score.path <- sprintf("%s/%s/score.dat", root.path, path)
    load(score.path)
    num.users <- nrow(score)
    
    user.test <- read.user(sprintf("%s/cf-test-%d-users.dat", root.path, i))
    
    recall.user.file <- sprintf("%s/%s/recall-user.dat", root.path, path)
    cat(sprintf("Computing %s...\n", recall.user.file))
    write("user.id,fold,N,recall", file=recall.user.file)
    
    for (j in 1:num.users) {
      user.items <- user.test[[j]]
      ids.left <- splits[[j]][[i]]
      score.u <- score[j, ids.left] 
      s <- sort.int(x=score.u, decreasing=TRUE, index.return=TRUE)
      idx <- s$ix[1:top]
      item.ids <- ids.left[idx]
      intest <- as.integer(sapply(item.ids, FUN=function(x) {x %in% user.items}))
      recall <- cumsum(intest)
      recall <- recall[breaks] # unnormalized
      write(rbind(j, i, breaks, recall), ncolumns=4, file=recall.user.file, append=T, sep=",")
    }
  }
}


#RUN in R

scoring.all(root.path="/home/valerio/processed_nips",num.topics=NUM_TOPICS)
compute.recall.all(root.path="/home/valerio/processed_nips/10K_5fold/")
summary.recall(root.path="/home/valerio/processed_nips/10K_5fold") #—> outputs average recall for different numbers of M recommended papers
