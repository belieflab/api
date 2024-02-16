

cleanDataFrameExists <- function(measure_alias, measure_type) {
  
  if(!require(tidyverse)) {install.packages("tidyverse")};library(tidyverse)
  
  # append _clean to the measure in question
  output_df_name <- paste0(measure_alias, "_clean")
  
  tryCatch({
    
    df_clean_exists <- test_that("Clean df exists", {
      
      # Check if the expected output_df_name is created
      df_clean_exists <- testthat::expect_true(exists(output_df_name), 
                                               info = paste("The script did not create '", output_df_name, "' dataframe."))
    })
    
  }, error = function(e) {
    message("The script did not create a'", output_df_name, "' dataframe.", e$message)
  })
  
}