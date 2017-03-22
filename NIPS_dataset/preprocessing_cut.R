#Create item.dat and user.dat
#Remove all papers liked by user 1, suppose this is a new user.
#(of course remove the corresponding entries from item.dat)

dati_nips10 <- read.table("/home/valerio/nips10/rev_paper_scores.csv", sep = ",",header = TRUE)
dati_nips10 <- dati_nips10[which(dati_nips10$score > 1),] # only keep the ones with a positive score 
dati_nips10_item <- aggregate(revID ~ paperID, data = dati_nips10, c)
dati_nips10_user <- aggregate(paperID ~ revID, data = dati_nips10, c)

j <- 1
for (i in 1:max(dati_nips10_item[,1])){ #to include items not liked by anyone (line with a 0 only)
  if (i == dati_nips10_item[j,1]){
    temp <- setdiff(as.numeric(unlist((dati_nips10_item[i,-1]))),1)
    len <- length(temp)
    line <- c(len,temp-1)
    write(line,file="/home/valerio/processed_nips/nips_items_cut.dat",append=TRUE,ncolumns=length(line))
    j <- j + 1
  } else{
    line <- 0
    write(line,file="/home/valerio/processed_nips/nips_items_cut.dat",append=TRUE,ncolumns=length(line))
  }
}

for (i in 1:dim(dati_nips10_user)[1]){
  if (i > 1){
  temp <- as.numeric(unlist(dati_nips10_user[i,-1]))
  len <- length(temp)
  line <- c(len,temp-1)
  } else{
    line <- 0
  }
  write(line,file="/home/valerio/processed_nips/nips_user_cut.dat",append=TRUE,ncolumns=length(line))
}
