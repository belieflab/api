add_prefix_to_columns <- function(df, collection_name, excluded_cols = c("src_subject_id", "visit", "phenotype", "interview_age", 
                                                                         "interview_date", "site", "sex", "subjectkey", "arm", "handedness", "state", "status")) {
  # Get the current column names
  col_names <- names(df)
  
  # Create a new set of column names
  new_col_names <- sapply(col_names, function(col) {
    # Check if column should be excluded from prefixing
    if (col %in% excluded_cols) {
      return(col)
    }
    
    # Check if the column name already starts with collection_name
    if (!startsWith(col, paste0(collection_name, "_"))) {
      # If not, add the prefix
      return(paste0(collection_name, "_", col))
    } else {
      # If it does, keep it as is
      return(col)
    }
  })
  
  # Rename the columns
  names(df) <- new_col_names
  
  return(df)
}