
checkNan <- function(measure_alias) {
  
  if (!require(testthat)) {install.packages("testthat")}; library(testthat)
  
  output_df_name <- paste0(measure_alias, "_clean")
  df_clean <- base::get(output_df_name)
  
  any_na <- which(is.na(df_clean), arr.ind = TRUE)
  tryCatch({
    test_that("Check for NA values", {
      if (length(any_na) > 0) {
        message("NA values found:")
        for (i in 1:nrow(any_na)) {
          row_idx <- any_na[i, 1]
          col_idx <- any_na[i, 2]
          src_subject_id <- df_clean$src_subject_id[row_idx]
          column_name <- colnames(df_clean)[col_idx]
          message(paste("src_subject_id:", src_subject_id, "- Column:", column_name))
        }
        testthat::expect_false(TRUE, 
                               info = paste("NA values found. src_subject_id:", df_clean$src_subject_id[any_na[,1]], "- Column:", colnames(df_clean)[any_na[,2]]))
      } else {
        expect_true(TRUE, info = "No NA values should be present in the dataframe.")
      }
    }, error = function(e) {
      message("CheckNA failed: ", e$message)
      
    }
    
    )
  })
}
