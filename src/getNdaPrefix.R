getNdaPrefix <- function(script_path) {
  
  base::source(script_path)
  return(nda_prefix)
  
}