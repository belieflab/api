# Get full file paths of all R files in the api directory
# base::source all files using lapply()

dataMerge <- function(..., by = c("src_subject_id","phenotype"), all = TRUE) {
  
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  
  data_list <- list(...)
  
  # if in data_list:
  # source("clean/task/complete/mooney.R")
  # source("clean/qualtrics/complete/rgpts.R")
  # source("clean/qualtrics/complete/pss.R")
  
  dfs <- Reduce(function(x, y) base::merge(x, y, by = by, all = all), data_list)
  
  # Clean Up
  suppressWarnings(source("api/env/cleanup.R"))

  return(dfs)
}



