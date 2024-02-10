### Collaborators (auto-populated):
###
createRds <- function(df) {
  path = paste0("extracts/",deparse(substitute(df)),'.Rda')
  saveRDS(df,file=path)
  
  message(paste0("Extract created at ",path,"\n"))
}
