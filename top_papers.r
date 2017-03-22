# Find top 3 papers for each topic
# Valerio Perrone

gammas <- read.table("/homes/perrone/ctr/ICML2016/results50/final.gamma", sep = " ")
top_papers <- apply(gammas,2,function(x) order(x,decreasing=TRUE)[1:3])
files <- list.files("/homes/perrone/ctr/ICML2016/icml_pdf_conversion/icml2016/txt_files/",pattern =".txt")
output <- apply(top_papers,2,function(x)   files[x]  )

write.table(output,"/homes/perrone/ctr/ICML2016/results50/top_papers50.txt")