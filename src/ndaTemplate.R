ndaTemplate <- function(df) {
  if (!dir.exists("nda")) {
    dir.create("nda")
  }
  
  # Define structure_name explicitly
  structure_name <- df  # Assuming '01' is static
  
  # Create the file path
  path <- file.path('nda/tmp', paste0(df, '_template.csv'))
  
  # Get the dataframe
  template <- base::get(df, envir = .GlobalEnv)
  
  # Open a connection to overwrite the file
  con <- file(path, "w")
  
  # Split structure name into base name and suffix
  structure_short_name <- substr(structure_name, 1, nchar(structure_name) - 2)  # gets "eefrt"
  structure_suffix <- substr(structure_name, nchar(structure_name) - 1, nchar(structure_name))  # gets "01"
  
  # Write the line with separated components
  writeLines(paste0(structure_short_name, ",", structure_suffix), con)
  
  # Write column headers manually
  writeLines(paste(names(template), collapse = ","), con)
  
  # Close the connection to save changes
  close(con)
  
  # Append the data without column headers
  write.table(template, path, row.names = FALSE, col.names = FALSE, append = TRUE, 
              quote = TRUE, sep = ",", na = "")
  
  message(paste0("\nSubmission Template created at: ", path, "\n"))
}
