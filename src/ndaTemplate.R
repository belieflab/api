ndaTemplate <- function(df, nda_prefix) {
  # Create the file path using file.path for platform independence
  path <- file.path('extracts', paste0(nda_prefix, '01_template.csv'))
  
  # Write the first line to the CSV as text
  first_line <- paste(nda_prefix, "01", sep = ",")
  write(first_line, path, append = FALSE)
  
  # Append the data frame to the CSV
  write.table(df, path, row.names = FALSE, append = TRUE, quote = FALSE, sep=",")
  
  cat(paste0("Extract created at ", path))
}
