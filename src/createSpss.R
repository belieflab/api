if(!require(foreign)) {install.packages("foreign")}; library(foreign);

createSpss <- function(df) {
  path = paste0("extracts/",deparse(substitute(df)),'.sav')
  write.foreign(df, datafile = path,
                      codefile = tempfile(), package = "SPSS")
  
  message(paste0("Extract created at ",path,"\n"))

}

