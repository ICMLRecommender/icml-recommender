#!/usr/bin/Rscript --slave

# install required packages
pkgs = c("tidyverse", 
         "rvest",
         "jsonlite",
         "yaml",
         "sofa",
         "rjson")

for (i in seq_along(pkgs)) {
  if (!require(pkgs[i], character.only = TRUE))
    install.packages(pkgs[i], dependencies = TRUE,
                     repos="http://cran.rstudio.com/")
}
