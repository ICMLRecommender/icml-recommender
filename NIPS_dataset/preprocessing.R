# Preprocess NIPS10 dataset
# Author: Valerio Perrone
# March 2017



#Create mult_user.dat and mult_user.dat

mult_nips10 <- read.table("/home/valerio/nips10/bow_papers.csv", sep = ",",header = TRUE)
mult_nips10_bypaper <- aggregate(cbind(wordID,count)~paperID, data = mult_nips10, c)

for (i in 1:dim(mult_nips10_bypaper)[1]){
  temp <- paste(as.numeric(unlist(mult_nips10_bypaper[i,-1][1]))-1, as.numeric(unlist(mult_nips10_bypaper[i,-1][2])),sep=":")
  len <- length(temp)
  line <- c(len,temp)
  write(line,file="/home/valerio/processed_nips/nips_mult_item.dat",append=TRUE, ncolumns=length(line))
}


mult_nips10 <- read.table("/home/valerio/nips10/bow_revs.csv", sep = ",",header = TRUE)
mult_nips10_byrev <- aggregate(cbind(wordID,count)~revID, data = mult_nips10, c)

for (i in 1:dim(mult_nips10_byrev)[1]){
  temp <- paste(as.numeric(unlist(mult_nips10_byrev[i,-1][1]))-1, as.numeric(unlist(mult_nips10_byrev[i,-1][2])),sep=":")
  len <- length(temp)
  line <- c(len,temp)
  write(line,file="/home/valerio/processed_nips/nips_mult_user.dat",append=TRUE, ncolumns=length(line))
}




#Create item.dat and user.dat

dati_nips10 <- read.table("/home/valerio/nips10/rev_paper_scores.csv", sep = ",",header = TRUE)
dati_nips10 <- dati_nips10[which(dati_nips10$score > 1),] # only keep the ones with a positive score 
dati_nips10_item <- aggregate(revID ~ paperID, data = dati_nips10, c)
dati_nips10_user <- aggregate(paperID ~ revID, data = dati_nips10, c)

j <- 1
for (i in 1:max(dati_nips10_item[,1])){ #to include items not liked by anyone (line with a 0 only)
  if (i == dati_nips10_item[j,1]){
    temp <- as.numeric(unlist((dati_nips10_item[i,-1])))
    len <- length(temp)
    line <- c(len,temp-1)
    write(line,file="/home/valerio/processed_nips/nips_items.dat",append=TRUE,ncolumns=length(line))
    j <- j + 1
  } else{
    line <- 0
    write(line,file="/home/valerio/processed_nips/nips_items.dat",append=TRUE,ncolumns=length(line))
  }
}

for (i in 1:dim(dati_nips10_user)[1]){
  temp <- as.numeric(unlist(dati_nips10_user[i,-1]))
  len <- length(temp)
  line <- c(len,temp-1)
  write(line,file="/home/valerio/processed_nips/nips_user.dat",append=TRUE,ncolumns=length(line))
}
