

## main ##

ndaSuite <- function(measure_alias, measure_type, df) {
  lapply(list.files("api/test", pattern = "\\.R$", full.names = TRUE), base::source)
  
  
  if (!require(testthat)) {install.packages("testthat")}; library(testthat)
  
  ndaCheckQualtricsDuplicates(measure_alias, measure_type, df)
  
  ndaRequiredSubmissionVariablesExist(measure_alias, measure_type, df)
  

  
}
