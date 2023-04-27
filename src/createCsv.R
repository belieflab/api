createCsv <- function(df) {
  path = paste('extracts/',deparse(substitute(df)),'.csv',sep='')
  write.csv(df,path,row.names = FALSE)
  cat(paste0("Extract created at ",path))
}