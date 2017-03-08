# Recommends top papers to each user given U and V
# (latent user and item representations found by CTR)

# Author: Valerio Perrone

# Inputs (to be given from command line in this order): 
# path = path to final-U.dat and final-V.dat
# num.topics = number of latent factors K  
# top = number of top papers to recommend
# Output: saves in path the file recommendations.txt: matrix (user x top) ids of recommended papers
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
  
recommendations = matrix(0,dim(score)[1],top)  # users x top
for (j in 1:dim(score)[1]){
    score.u <- score[j,] 
    s <- sort.int(x=score.u, decreasing=TRUE, index.return=TRUE)
    recommendations[j,] <- s$ix[1:top]
}
  
rec.path <- sprintf("%s/recommendations.txt", path)
write.table(recommendations, rec.path, sep="\t")
