################## test 4 ##################

checkColumnPrefix <- function(measure_alias, measure_type) {
  
  if(!require(tidyverse)) {install.packages("tidyverse")};library(tidyverse)
  
  # Construct dataframe name based on measure_alias
  df_name <- paste0(measure_alias, "_clean")
  
  # Fetch the dataframe
  df <- base::get(df_name)
  
  # List of columns to remove
  cols_to_remove <- c("StartDate", "EndDate", "Status", "Progress", "Duration (in seconds)", 
                      "Finished", "RecordedDate", "ResponseId", "DistributionChannel",
                      "UserLanguage", "candidateId", "studyId", "measure", "ATTN", "ATTN_1", "SC0")
  
  # Remove columns only if they exist in df
  df <- df %>% select(-any_of(cols_to_remove))
  
  # Assuming 'nda_required' and 'responseId' are correctly defined lists or vectors of column names
  if (measure_type == "qualtrics") {
    non_nda_cols <- setdiff(colnames(df), c(nda_required_variables, "responseId"))  # assuming 'responseId' is the correct column name
  } else {
    non_nda_cols <- setdiff(colnames(df), nda_required_variables)
  }
  
  # Now 'non_nda_cols' will have the columns you want to keep or process separately
  
  
  # Check naming convention for non-NDA columns
  test_that("Check column naming convention", {
    expect_true(all(grepl(paste0("^", measure_alias, "_"), non_nda_cols)), 
                info = paste("Non-NDA columns in '", df_name, 
                             "' follow the correct naming convention."))
  })
}


