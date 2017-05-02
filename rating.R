#!/usr/bin/Rscript

# Recommends top papers to each user given U and V
# (latent user and item representations found by CTR)
# Note: modified to exclude liked papers

# Author: Valerio Perrone

# Inputs (to be given from command line in this order): 
# path = path to final-U.dat, final-V.dat and users.dat
# num.topics = number of latent factors K  
# top = upper bound of top papers to recommend (upper bound as the papers that have been already liked are removed from top)
# Output: saves in path the file recommendations.txt: list of ids of recommended papers for each user
# example run: Rscript rating.R results/ 200 4


args <- commandArgs(trailingOnly = TRUE)
if (length(args)<3) {
  stop("Three arguments required (in this order): 1. path to final-U.dat and final-V.dat 2. number of factors 3. number of papers to recommend per user", call.=FALSE)
} 
path <- args[1]
num.topics <- as.integer(args[2])
top <- as.integer(args[3])


u.path <- sprintf("%s/final-U.dat", path)
u <- scan(u.path)
u <- matrix(u, nrow=num.topics)  # K x users 
v.path <- sprintf("%s/final-V.dat", path)
v <- scan(v.path)
v <- matrix(v, nrow=num.topics) # K x items
score <- t(u) %*% v # users x items

read.user <- function(filename, offset=1)
{
  one <- scan(filename, what = "", sep = "\n", quiet=T)
  two <- strsplit(one, " ", fixed = TRUE)
  lapply(two, function(x) (offset+as.vector(as.integer(x[-1]))))
}


all.users <- read.user(sprintf("%s/users.dat", path))

recommendations = vector("list", dim(score)[1]) 
for (j in 1:dim(score)[1]){
  user.items <- all.users[[j]]
  score.u <- score[j,]  
  s <- sort.int(x=score.u, decreasing=TRUE, index.return=TRUE) 
  all_recommended_IDs <- s$ix[1:top]
  recommendations[[j]] <- setdiff(all_recommended_IDs,user.items) ##remove papers already liked from recommendations 
}

rec.path <- sprintf("%s/recommended_papers.txt", path)
lapply(recommendations, write, rec.path, append=TRUE, ncolumns=600)