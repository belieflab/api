
ndaRequiredVariablesExist <- function(measure_alias, measure_type, nda_required_variables) {
  
  if (!require(testthat)) {install.packages("testthat")}; library(testthat)
  
  # append _clean to the measure in question
  output_df_name <- paste0(measure_alias, "_clean")
  
  # store clean dataframe in df_clean
  df_clean <- base::get(output_df_name)
  
  # Initial list of NDA required variables
  nda_required_variables <- c("src_subject_id", "phenotype", "site", "visit", "week", 
                              "subjectkey", "sex", "interview_date", "interview_age")
  
  # Adjust NDA required variables based on presence of 'visit' or 'week'
  adjusted_nda_required <- nda_required_variables
  
  # If 'visit' and 'week' are not both required, adjust the list accordingly:
  if (!("visit" %in% colnames(df_clean)) && ("week" %in% colnames(df_clean))) {
    adjusted_nda_required <- setdiff(adjusted_nda_required, "visit")  # Remove 'visit' if it's not there but 'week' is
  } else if (("visit" %in% colnames(df_clean)) && !("week" %in% colnames(df_clean))) {
    adjusted_nda_required <- setdiff(adjusted_nda_required, "week")  # Remove 'week' if it's not there but 'visit' is
  } # If neither or both are present, no changes needed to adjusted_nda_required
  
  # Now check if the output dataframe contains all adjusted NDA required variables
  missing_vars <- setdiff(adjusted_nda_required, colnames(df_clean))
  
  tryCatch({
    test_that("Check for missing NDA required variables", {
      testthat::expect_true(length(missing_vars) == 0, 
                            info = paste("All NDA required variables are not present in '", measure_alias, " please make sure the following variable is present in the clean df: '", missing_vars, "."))
    })
  }, error = function(e) {
    message("All NDA required variables are not present in '", measure_alias, " please make sure the following variable is present in the clean df: '", missing_vars, ".", e$message)
  })
  
}