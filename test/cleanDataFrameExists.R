

cleanDataFrameExists <- function(measure_alias, measure_type) {
  
  if (!require(testthat)) {install.packages("testthat")}; library(testthat)
  
  # append _clean to the measure in question
  output_df_name <- paste0(measure_alias, "_clean")
  
  tryCatch({
    
    test_that("Clean df exists", {
      
      # Check if the expected output_df_name is created
      testthat::expect_true(exists(output_df_name), 
                                               info = paste("The script did not create '", output_df_name, "' dataframe."))
    })
    
  }, error = function(e) {
    message("The script did not create a'", output_df_name, "' dataframe.", e$message)
  })
  
}