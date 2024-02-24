#' Check for Duplicates in Qualtrics Data
#'
#' This function checks for duplicate records in a specified Qualtrics dataset
#' based on certain identifiers and time points. If duplicates are found,
#' they are exported to a CSV file for review and additionally displayed in the viewer.
#'
#' @param measure_alias A string representing the name of the dataset to check for duplicates.
#' @param measure_type A string specifying the type of measure, currently supports 'qualtrics' only.
#' @return The function does not return a value but will generate a CSV file if duplicates are found
#'         and display those duplicates in the RStudio viewer.
#' @export
#' @examples
#' checkDuplicates("your_dataset_alias", "qualtrics")
#' @importFrom dplyr filter %>% 
#' @importFrom testthat test_that expect_true
#' @importFrom utils View stop
#' @note This function requires the dplyr and testthat packages. It is specifically designed for
#'       Qualtrics data and expects the data frame to be named with a '_clean' suffix.
#'       It checks for duplicates based on 'src_subject_id' combined with 'visit' or 'week' columns.
#'       The function will stop and throw an error if the necessary columns are not present.
checkQualtricsDuplicates <- function(measure_alias, measure_type) {

  # Ensure required packages are loaded
  if (!require(dplyr)) { install.packages("dplyr") }; library(dplyr)
  if (!require(testthat)) { install.packages("testthat") }; library(testthat)
  
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
            
            # export
            duplicate_extract <- paste0("duplicates_", measure_alias)

            # Use your custom function to create a CSV file
            createCsv(df_duplicates, paste0("duplicates_", measure_alias))
            
            # Wrap the test_that in a try-catch block
            tryCatch({
              test_that("Check for Qualtrics duplicates", {
                testthat::expect_true(nrow(df_duplicates) == 0, 
                            info = paste("Duplicates detected in '", measure_alias, "': ", 
                                         "Offending IDs: ", toString(unique(df_duplicates[[identifier]]))))
              })
            }, error = function(e) {
              # Convert the duplicate details into a readable format
              duplicates_summary <- toString(unique(df_duplicates[[identifier]]))
              message(paste("Error in testing for duplicates in '", measure_alias, "': ", e$message, 
                            "\nOffending IDs: ", duplicates_summary,
                            "\nDetails exported to ", paste0("./tmp/",duplicate_extract,".csv")))
            })
            
            # View the offending records in RStudio's data viewer
            View(df_duplicates)
          }
        }
      }
    }
  }
}
