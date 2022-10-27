#rm(list = ls())

setwd("C:/Users/Morgan/_data_projects/rpsc_scheduler")

main <- list()

main$func$require_packages <- function(packages){
  new.packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  
  lapply(packages,require,character.only = TRUE)
}

main$config$recrec_api_key <- readChar("recrec_api_key.txt"
                                       ,file.info("recrec_api_key.txt")$size)

main$config$get_new_recrec_data <- FALSE

if(!file.exists("./data/rec_data.RDS") |
                main$config$get_new_recrec_data
   ){  source("./getrecrec_data.R") }

