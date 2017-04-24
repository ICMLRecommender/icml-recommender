# install required packages
pkgs = c("tidyr", 
         "dplyr", 
         "readr", 
         "rvest",
         "jsonlite",
         "yaml",
         "sofa")

for (i in seq_along(pkgs)) {
  if (!require(pkgs[i], character.only = TRUE))
    install.packages(pkgs[i], dependencies = TRUE,
                     repos="http://cran.rstudio.com/")
}
