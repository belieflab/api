#### 5. test for capr standards for loading libraries example: if(!require(dplyr)) {install.packages("dplyr")}; library(dplyr);
# check if an if(!require exists?

# findTextInScript
# search for:
#   "Collaborators" (indicates githook was used; can then contact person/author)
#   "describe(" or "table(" or "ggplot(" (suggests someone looked at descriptive stats or data distribution)
#   "rm("
#   "getDictionary("
#   "checkRedcapDuplicates"
#   "sum("
#   "mean(" or "rowMeans(" or "summarize" or "summarise" (indicates that scales, indices, or summary variables have been computed)
#   "

findTextInScript <- function(script_path, text_to_search) {
  
  if (!require(testthat)) {install.packages("testthat")}; library(testthat)
  
  script_contents <- base::readLines(script_path)
  
  # Check if any of the strings are present in the script
  matches_grep <- any(sapply(text_to_search, function(text) any(grepl(text, script_contents, fixed = TRUE))))
  
  # Print the result
  print(matches_grep)
  
  tryCatch({
    # Create a test that fails only if none of the strings are found
    test_that(paste0("At least one of ", toString(text_to_search), " is present"), {
      testthat::expect_true(matches_grep, 
                  info = paste(toString(text_to_search), " don't appear in your script."))
    }, error = function(e) {
      message("FindTextInScript failed: ", e$message)
      
    }
    
    )
  })
}
