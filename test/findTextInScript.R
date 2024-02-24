#' Find Specific Text within a Script File
#'
#' This function searches for specified text or phrases within a script file. It is useful for checking
#' if certain functions or terms are present, which can indicate adherence to specific coding standards or
#' practices, such as data exploration, cleanup, or specific analyses.
#'
#' @param script_path The file path of the R script to be checked.
#' @param text_to_search A vector of strings representing the text or code snippets to search for within the script.
#' @return This function prints a boolean result indicating whether any of the specified text or phrases were found
#'         in the script. Additionally, it uses `testthat` to assert the presence of at least one of the specified
#'         text snippets and provides feedback accordingly.
#' @export
#' @examples
#' findTextInScript("path/to/your/script.R", c("Collaborators", "describe(", "table(", "ggplot(", "rm(", "getDictionary(", "checkRedcapDuplicates", "sum(", "mean(", "rowMeans(", "summarize", "summarise"))
#' @importFrom testthat test_that expect_true
#' @importFrom base readLines
#' @note This function is part of a suite of tools intended to enforce coding standards and practices.
#'       It's particularly useful for code reviews or automated checks as part of a continuous integration process.
#' @todo
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
