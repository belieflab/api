checkDuplicates <- function(measure_alias, measure_type) {
  # Ensure required packages are loaded
  if (!require(dplyr)) { install.packages("dplyr") }
  library(dplyr)
  if (!require(testthat)) { install.packages("testthat") }
  library(testthat)
  
  # Generate the name of the dataframe and get it
  output_df_name <- paste0(measure_alias, "_clean")
  df <- base::get(output_df_name)
  
  identifier <- "src_subject_id"
  
  if (measure_type == "qualtrics") {
    if (!(identifier %in% colnames(df))) {
      stop("Please provide a valid identifier: src_subject_id, workerId, PROLIFIC_PID")
    }
    
    for (col in c("visit", "week")) {
      if (col %in% colnames(df)) {
        df$duplicates <- duplicated(df[c(identifier, col)], first = TRUE)
        
        # Separate the duplicates to their own df
        df_dup_ids <- subset(df, duplicates == TRUE)[, c(identifier, col)]
        
        if (nrow(df_dup_ids) > 0) {
          df_duplicates <- df %>% filter((.[[identifier]] %in% df_dup_ids[[identifier]]) & (.[[col]] %in% df_dup_ids[[col]]))
          
          # Check for duplicates and create a CSV file if found
          if (nrow(df_duplicates) > 0) {

            # Use your custom function to create a CSV file
            createCsv(df_duplicates, paste0("duplicates_", measure_alias))
            
            # Wrap the test_that in a try-catch block
            tryCatch({
              test_that("Check for Qualtrics duplicates", {
                expect_true(nrow(df_duplicates) == 0, 
                            info = paste("Duplicates detected in '", measure_alias, "': ", 
                                         "Offending IDs: ", toString(unique(df_duplicates[[identifier]]))))
              })
            }, error = function(e) {
              # Convert the duplicate details into a readable format
              duplicates_summary <- toString(unique(df_duplicates[[identifier]]))
              message(paste("Error in testing for duplicates in '", measure_alias, "': ", e$message, 
                            "\nOffending IDs: ", duplicates_summary))
            })
            
            # View the offending records in RStudio's data viewer
            View(df_duplicates)
          }
        }
      }
    }
  }
}
