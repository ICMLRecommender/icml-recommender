#!/usr/bin/Rscript --slave

# install required packages
pkgs = c("tidyverse", 
         "rvest",
         "jsonlite",
         "sofa",
         "rjson",
         "tidytext",
         "topicmodels",
         "ggraph")

for (i in seq_along(pkgs)) {
  if (!require(pkgs[i], character.only = TRUE))
    install.packages(pkgs[i], dependencies = TRUE,
                     repos="http://cran.rstudio.com/")
}

if (!require(pluralize, quietly = TRUE)) {
  devtools::install_github("hrbrmstr/pluralize")
}

if (!require(ggpage, quietly = TRUE)) {
  devtools::install_github("EmilHvitfeldt/ggpage")
}
