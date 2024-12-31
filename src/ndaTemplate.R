ndaTemplate <- function(df) {
  if (!dir.exists("nda")) {
    dir.create("nda")
  }
  
  # Define structure_name explicitly
  structure_name <- df  # Assuming '01' is static
  
  # Create the file path
  path <- file.path('nda', paste0(df, '01_template.csv'))
  
  # Get the dataframe
  template <- base::get(df, envir = .GlobalEnv)
  
  # Open a connection to overwrite the file
  con <- file(path, "w")
  
  # Write the first line with "structure_name, 01"
  writeLines(paste0(structure_name, ",01"), con)
  
  # Write column headers manually
  writeLines(paste(names(template), collapse = ","), con)
  
  # Close the connection to save changes
  close(con)
  
  # Append the data without column headers
  write.table(template, path, row.names = FALSE, col.names = FALSE, append = TRUE, 
              quote = TRUE, sep = ",", na = "")
  
  message(paste0("\nSubmission Template created at: ", path, "\n"))
}
