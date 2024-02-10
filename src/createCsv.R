createCsv <- function(df) {
  path = paste0('extracts/',deparse(substitute(df)),'.csv')
  write.csv(df,path,row.names = FALSE)
  message(paste0("Extract created at ",path,"\n"))
}