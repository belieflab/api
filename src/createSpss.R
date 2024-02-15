if(!require(haven)) {install.packages("haven")}; library(haven);

createSpss <- function(df) {
  path = paste0("./tmp/", deparse(substitute(df)), '.sav')
  write_sav(df, path)
  
  message(paste0("Extract created at ", path, "\n"))
}

