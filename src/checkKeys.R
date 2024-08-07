checkKeys <- function(df_name, variables, type = "data") {
  # Attempt to retrieve the dataframe from the global environment
  df <- base::get(df_name)
  
  # Find any missing variables
  missing_vars <- setdiff(variables, names(df))
  # Find present variables
  present_vars <- intersect(variables, names(df))
  
  # Print present variables
  if (length(present_vars) > 0) {
    message(paste("Candidate keys present in ", type, " data of ", df_name, ": ", paste(present_vars, collapse=", "), "."))
  }
  
  # # Handle missing variables
  # if (length(missing_vars) > 0) {
  #   stop(paste("Missing variables in ", type, " data of ", df_name, ": ", paste(missing_vars, collapse=", "), "."))
  # } else {
  #   message(paste("All required variables are present in ", type, " data of ", df_name, "."))
  # }
  # 
  
  # Handle missing variables with tryCatch
  tryCatch({
    if (length(missing_vars) > 0) {
      # Instead of stopping, just print the message about missing variables
      message(paste("Missing variables in", type, "data of", df_name, ":", paste(missing_vars, collapse=", "), ". Merging on present variables:", paste(present_vars, collapse=", "), "."))
    } else {
      # Only print this message if there are no missing variables
      message(paste("All required variables are present in ", type, " data of ", df_name, "."))
    }
  }, error = function(e) {
    message("An unexpected error occurred: ", e$message)
  })
  # Return the list of present variables for further checks
  return(present_vars)
}
