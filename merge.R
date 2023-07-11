source("clean/task/complete/mooney.R")
source("clean/qualtrics/complete/rgpts.R")
source("clean/qualtrics/complete/pss.R")

multiMerge <- function(..., by = c("src_subject_id"), all = TRUE) {
  data_list <- list(...)
  
  # if in data_list:
  # source("clean/task/complete/mooney.R")
  # source("clean/qualtrics/complete/rgpts.R")
  # source("clean/qualtrics/complete/pss.R")
  
  dfs <- Reduce(function(x, y) base::merge(x, y, by = by, all = all), data_list)
  View(dfs)
  
}

multiMerge(mooney_clean, rgpts_clean, pss_clean)


