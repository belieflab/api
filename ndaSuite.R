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
    
    if (all(!c("visit", "week") %in% colnames(df))) {
      df$duplicates <- duplicated(df$identifier, first = TRUE)
      
      # separate the duplicates to their own df
      df_dup_ids <- subset(df, duplicates == TRUE)$identifier
      
      # filter only the subject ids that are duplicated to include both iterations
      df_duplicates <- df %>% filter(identifier %in% df_dup_ids)
      test_that("Check for Qualtrics duplicates", {
        expect_true(nrow(df_duplicates) == 0, 
                    info = paste("Duplicates detected in '", measure_alias, "'."))
      })
    }
    
    if ("visit" %in% colnames(df)) {
      df$duplicates <- duplicated(df[c(identifier, "visit")], first = TRUE)
      
      # separate the duplicates to their own df
      df_dup_ids <- subset(df, duplicates == TRUE)[c(identifier, "visit")]
      
      # filter only the subject ids that are duplicated to include both iterations
      df_duplicates <- df %>% filter(identifier %in% df_dup_ids & visit %in% df_dup_ids)
      test_that("Check for Qualtrics duplicates", {
        expect_true(nrow(df_duplicates) == 0, 
                    info = paste("Duplicates detected in '", measure_alias, "'."))
      })
    }
    
    if ("week" %in% colnames(df)) {
      df$duplicates <- duplicated(df[c(identifier, "week")], first = TRUE)
      
      # separate the duplicates to their own df
      df_dup_ids <- subset(df, duplicates == TRUE)[c(identifier, "week")]
      
      # filter only the subject ids that are duplicated to include both iterations
      df_duplicates <- df %>% filter(identifier %in% df_dup_ids & week %in% df_dup_ids)
      test_that("Check for Qualtrics duplicates", {
        expect_true(nrow(df_duplicates) == 0, 
                    info = paste("Duplicates detected in '", measure_alias, "'."))
      })
    }
    
    
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
