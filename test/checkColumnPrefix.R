################## test 4 ##################

checkColumnPrefix <- function(measure_alias, measure_type, nda_required_variables) {
  
  if (!require(testthat)) {install.packages("testthat")}; library(testthat)
  if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
  
  # Construct dataframe name based on measure_alias
  df_name <- paste0(measure_alias, "_clean")
  
  # Fetch the dataframe
  df <- base::get(df_name)
  
  # Determine columns to check based on measure_type
  if (measure_type == "qualtrics") {
    non_nda_cols <- setdiff(colnames(df), c(nda_required_variables, "ResponseId"))
  } else {
    non_nda_cols <- setdiff(colnames(df), nda_required_variables)
  }
  
  tryCatch({
    # Check naming convention for non-NDA columns
    test_that("Check column naming convention", {
      testthat::expect_true(
        all(grepl(paste0("^", measure_alias, "_"), non_nda_cols)),
        info = paste("Some non-NDA columns in '", df_name, 
                     "' do not follow the correct naming convention starting with ", measure_alias, "_")
      )
    })
  }, error = function(e) {
    message(paste("Some non-NDA columns in '", df_name, 
                  "' do not follow the correct naming convention starting with ", measure_alias, "_"), e$message)
  })
}
