ndaTemplate <- function(df) {
  if (!dir.exists("nda")) {
    dir.create("nda")
  }
  
  structure_name <- paste0(df, "01")
  
  # Create the file path
  path <- file.path('nda', paste0(structure_name, '_template.csv'))
  
  # Get the dataframe
  template <- base::get(df, envir = .GlobalEnv)
  
  # Write structure name as first line
  write(structure_name, path, append = FALSE)
  
  # Write column headers and data separately
  write.table(template, path, row.names = FALSE, col.names = TRUE, append = TRUE, 
              quote = FALSE, sep = ",", na = "")
  
  cat(paste0("Upload Template created at ", path,"\n"))
}