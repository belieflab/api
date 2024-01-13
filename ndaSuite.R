#
# test suite for NDA uploads
#
# functional requirements:
# 1) no duplicates
# 2) all NDA required variables are present
# 3) all recommended NDA variables are present according to each measure's data definitions template 
#

## requirement 1 ##

checkDuplicates <- function(measure_alias, measure_type, df) {
  
  identifier <- "src_subject_id"
  
  if (measure_type == "qualtrics") {
    
    if (!require(dplyr)) { install.packages("dplyr") }; library(dplyr)
    
    # Check if identifier column exists, if not, prompt for a valid identifier
    if (!(identifier %in% colnames(df))) {
      stop("Please provide a valid identifier: src_subject_id, workerId, PROLIFIC_PID")
    }
    
    # Define columns to exclude for duplicate checks
    exclusion_cols <- c("visit", "week")
    cols_to_check <- setdiff(colnames(df), c(identifier, exclusion_cols))
    
    # Check if any columns to compare for duplicates exist
    if (length(cols_to_check) == 0) {
      message("No columns found to check for duplicates apart from 'visit' and 'week'.")
      return(invisible())
    }
    
    df$duplicates <- duplicated(df[, cols_to_check, drop = FALSE], fromLast = FALSE)
    df_duplicates <- df[df$duplicates, ]
    
    test_that("Check for Qualtrics duplicates", {
      expect_true(nrow(df_duplicates) == 0, 
                  info = paste("No duplicates detected in '", measure_alias, "'."))
    })
  }
  
}

## requirement 2 ##

ndaRequiredVariablesExist <- function(measure_alias, measure_type, df) {

  # List of NDA required variables
  nda_req_var <- c("subjectkey", "src_subject_id", "interview_date", "interview_age", "sex")
  
  # Check if the output dataframe contains all NDA required variables
  missing_vars <- setdiff(nda_req_var, colnames(df))
  
  test_that("Check for missing NDA required variables", {
    expect_true(length(missing_vars) == 0, 
                info = paste("All NDA required variables are present in '", measure_alias, "'."))
  })
  
}

## main ##

ndaSuite <- function(measure_alias, measure_type, df) {
  
  if (!require(testthat)) {install.packages("testthat")}; library(testthat)
  
  checkDuplicates(measure_alias, measure_type, df)
  
  ndaRequiredVariablesExist(measure_alias, measure_type, df)

  
}
