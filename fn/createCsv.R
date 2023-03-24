createCsv <- function(df) {
  write.csv(df,paste('extracts/',deparse(substitute(df)),'_duplicates','.csv',sep=''),row.names = FALSE)
}