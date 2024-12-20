ndaTemplate <- function(df) {
  if (!dir.exists("nda")) {
    dir.create("nda")
  }
  
  # Define structure_name explicitly
  structure_name <- df  # No additional formatting here, assuming '01' is static
  
  # Create the file path
  path <- file.path('nda', paste0(df, '_template.csv'))
  
  # Get the dataframe
  template <- base::get(df, envir = .GlobalEnv)
  
  # Write the first line with "structure_name, 01"
  write(paste0(structure_name, ", 01"), path, append = FALSE)
  
  # Write column headers and data below
  write.table(template, path, row.names = FALSE, col.names = TRUE, append = TRUE, 
              quote = TRUE, sep = ",", na = "")
  
  cat(paste0("Upload Template created at ", path, "\n"))
}
