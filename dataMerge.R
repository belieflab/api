# Get full file paths of all R files in the api directory
# base::source all files using lapply()

dataMerge <- function(..., by = c("src_subject_id","subjectkey","phenotype","visit","sex","site"), all = FALSE) {
  
  # inner join = FALSE
  # outer join = TRUE
  
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  
  data_list <- list(...)
  
  dfs <- Reduce(function(x, y) base::merge(x, y, by = by, all = FALSE), data_list)
  
  # Clean Up
  suppressWarnings(source("api/env/cleanup.R"))

  return(dfs)
}



