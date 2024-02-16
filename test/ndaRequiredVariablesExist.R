
ndaRequiredVariablesExist <- function(measure_alias, measure_type) {
  
  if(!require(tidyverse)) {install.packages("tidyverse")};library(tidyverse)
  
  # append _clean to the measure in question
  output_df_name <- paste0(measure_alias, "_clean")
  
  # store clean dataframe in df_clean
  df_clean <- base::get(output_df_name)
  
  # Check if the output dataframe contains all NDA required variables
  missing_vars <- setdiff(nda_required_variables, colnames(df_clean))
  
  tryCatch({
    test_that("Check for missing NDA required variables", {
      testthat::expect_true(length(missing_vars) == 0, 
                            info = paste("All NDA required variables are not present in '", measure_alias, " please make sure the following variable is present in the clean df: '", missing_vars, "."))
    })
  }, error = function(e) {
    message("All NDA required variables are not present in '", measure_alias, " please make sure the following variable is present in the clean df: '", missing_vars, ".", e$message)
  })
  
}