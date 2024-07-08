

ndaRequiredSubmissionVariablesExist <- function(measure_alias, measure_type, df) {
  
  # List of NDA required variables
  nda_req_var <- c("subjectkey", "src_subject_id", "interview_date", "interview_age", "sex")
  
  # Check if the output dataframe contains all NDA required variables
  missing_vars <- setdiff(nda_req_var, colnames(df))
  tryCatch({
  test_that("Check for missing NDA required variables", {
    expect_true(length(missing_vars) == 0, 
                info = paste("All NDA required variables are present in '", measure_alias, "'."))
  })
    },error = function(e) {
    # Convert the duplicate details into a readable format
    message(paste("Check for missing vars in  '", missing_vars))
  })
  
}