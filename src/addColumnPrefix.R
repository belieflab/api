addColumnPrefix <- function(df) {
  # Determine the prefix from the dataframe name, excluding "_clean"
  prefix <- sub("_clean$", "", deparse(substitute(df)))
  
  # Specify columns that should not have the prefix added
  excluded_cols <- c("src_subject_id", "visit", "phenotype", "interview_age", "interview_date", "site", "sex", "subjectkey")
  
  # Identify columns that are not in the excluded list and don't already start with the prefix
  cols_to_prefix <- names(df)[!names(df) %in% excluded_cols & !startsWith(names(df), prefix)]
  
  # Loop through each column name that needs the prefix added
  new_names <- setNames(names(df), names(df))
  for (col in cols_to_prefix) {
    new_names[col] <- paste0(prefix, "_", col)
  }
  
  # Rename the columns in the dataframe
  names(df) <- new_names
  
  return(df)
}

# Example of usage:
# Assuming `social_prl_clean` is your dataframe
# social_prl_clean <- addColumnPrefix(social_prl_clean)
