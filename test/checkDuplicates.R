checkDuplicates <- function(measure_alias, measure_type) {
  
  # append _clean to the measure in question
  output_df_name <- paste0(measure_alias, "_clean")
  
  # store clean dataframe in df_clean
  df <- base::get(output_df_name)
  
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
        expect_true(nrow(df_duplicates) != 0, 
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
        expect_true(nrow(df_duplicates) != 0, 
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
        expect_true(nrow(df_duplicates) != 0, 
                    info = paste("Duplicates detected in '", measure_alias, "'."))
      })
    }
    
    
  }
  
}
# 
# checkDuplicates <- function(measure_alias, measure_type) {
#   
#   if (!require(testthat)) {install.packages("testthat")}; library(testthat)
#   
#   # append _clean to the measure in question
#   output_df_name <- paste0(measure_alias, "_clean")
#   
#   # store clean dataframe in df_clean
#   df <- base::get(output_df_name)
#   
#   identifier <- "src_subject_id"
#   
#   if (measure_type == "qualtrics") {
#     
#     if (!require(dplyr)) {
#       install.packages("dplyr")
#     }
#     library(dplyr)
#     
#     # Check if identifier column exists, if not, prompt for a valid identifier
#     if (!(identifier %in% colnames(df))) {
#       stop(paste("Please provide a valid identifier: src_subject_id, workerId, PROLIFIC_PID",
#                  "\nDuplicates check skipped for '", measure_alias, "'."))
#     }
#     
#     # Define columns to exclude for duplicate checks
#     exclusion_cols <- c("visit", "week")
#     cols_to_check <- setdiff(colnames(df), c(identifier, exclusion_cols))
#     
#     # Check if any columns to compare for duplicates exist
#     if (length(cols_to_check) == 0) {
#       message("No columns found to check for duplicates apart from 'visit' and 'week'.",
#               "\nDuplicates check skipped for '", measure_alias, "'.")
#       return(invisible())
#     }
#     
#     df$duplicates <- duplicated(df[, cols_to_check, drop = FALSE], fromLast = FALSE)
#     df_duplicates <- df[df$duplicates, ]
#     
#     tryCatch({
#       
#       test_that("Check for Qualtrics duplicates", {
#         testthat::expect_true(nrow(df_duplicates) == 0, 
#                               info = paste("No duplicates detected in '", measure_alias, "'."))
#       })
#     }, error = function(e) {
#       message(paste("No duplicates detected in '", measure_alias, "'."), e$message)
#     })
#   }
# }