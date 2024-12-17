#' Request and Validate Data Against NDA Structure
#'
#' This function takes a measure name and validates the associated dataframe against 
#' the NDA data structure by checking field names, types, and requirements against the NDA API.
#'
#' @param measure_name Character string of the measure name to validate
#' @param api_base_url Character string of the NDA API base URL (optional)
#' @return List containing validation results and any mismatches found
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#'
#' @export
ndaValidator <- function(measure_name, 
                         api_base_url = "https://nda.nih.gov/api/datadictionary/v2") {
  
  # Get the dataframe from the global environment
  df <- base::get(measure_name, envir = .GlobalEnv)
  
  # Append 01 to the measure name for NDA structure
  structure_name <- paste0(measure_name, "01")
  
  # Required packages
  required_packages <- c("httr", "jsonlite", "dplyr")
  for(pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
  
  # Helper function to fetch structure elements
  fetch_structure_elements <- function(structure_name) {
    url <- sprintf("%s/datastructure/%s", api_base_url, structure_name)
    response <- GET(url)
    
    if (status_code(response) != 200) {
      stop("Failed to fetch structure elements: ", content(response, "text"))
    }
    
    content <- fromJSON(rawToChar(response$content))
    
    if (!"dataElements" %in% names(content)) {
      stop("Unexpected API response format - no dataElements found")
    }
    
    elements <- content$dataElements
    return(elements)
  }
  
  # Helper function to validate value ranges
  validate_value_range <- function(value, range_str) {
    if (is.null(range_str) || is.na(range_str) || range_str == "") return(TRUE)
    if (is.null(value) || length(value) == 0) return(TRUE)
    
    # Clean the range string
    range_str <- gsub(" ", "", range_str)
    
    # Handle different range formats
    if (grepl("::", range_str)) {
      # Numeric range (e.g., "0::100")
      range <- as.numeric(strsplit(range_str, "::")[[1]])
      return(all(value >= range[1] & value <= range[2], na.rm = TRUE))
    } else if (grepl(";", range_str)) {
      # Categorical values (e.g., "M;F;O;NR")
      valid_values <- trimws(strsplit(range_str, ";")[[1]])
      return(all(as.character(value) %in% valid_values, na.rm = TRUE))
    }
    
    return(TRUE)
  }
  
  # Main validation logic
  validate_structure <- function(df, elements) {
    results <- list(
      valid = TRUE,
      missing_required = character(0),
      value_range_violations = list(),
      unknown_fields = character(0)
    )
    
    # Get required fields and all valid fields
    required_fields <- elements$name[elements$required == "Required"]
    valid_fields <- elements$name
    
    # Check for unknown fields
    df_cols <- names(df)
    results$unknown_fields <- setdiff(df_cols, valid_fields)
    if (length(results$unknown_fields) > 0) {
      results$valid <- FALSE
    }
    
    # Check for required fields
    if (length(required_fields) > 0) {
      missing_required <- required_fields[!required_fields %in% df_cols]
      if (length(missing_required) > 0) {
        results$valid <- FALSE
        results$missing_required <- missing_required
      }
    }
    
    # Check value ranges for all present valid columns
    for (col in intersect(df_cols, valid_fields)) {
      element <- elements[elements$name == col, ]
      if (nrow(element) > 0 && !is.null(element$valueRange) && !is.na(element$valueRange) && 
          element$valueRange != "" && !validate_value_range(df[[col]], element$valueRange)) {
        results$valid <- FALSE
        results$value_range_violations[[col]] <- list(
          expected = element$valueRange,
          actual = paste(unique(df[[col]]), collapse = ", ")
        )
      }
    }
    
    return(results)
  }
  
  # Main execution
  tryCatch({
    # Fetch elements
    cat("Fetching NDA Data Structure for", structure_name, "...\n")
    elements <- fetch_structure_elements(structure_name)
    if (is.null(elements) || nrow(elements) == 0) {
      stop("No elements found in the structure definition")
    }
    
    # Validate the dataframe
    cat("Validating dataframe...\n")
    validation_results <- validate_structure(df, elements)
    
    # Print results
    cat("\nValidation Results:\n")
    cat("Valid:", validation_results$valid, "\n")
    
    if (length(validation_results$missing_required) > 0) {
      cat("\nMissing Required Fields:\n")
      cat(paste("-", validation_results$missing_required), sep = "\n")
    }
    
    if (length(validation_results$value_range_violations) > 0) {
      cat("\nValue Range Violations:\n")
      for (col in names(validation_results$value_range_violations)) {
        violation <- validation_results$value_range_violations[[col]]
        cat(sprintf("- %s: expected range %s, got values: %s\n",
                    col, violation$expected, violation$actual))
      }
    }
    
    if (length(validation_results$unknown_fields) > 0) {
      cat("\nUnknown Fields (not in NDA structure):\n")
      cat(paste("-", validation_results$unknown_fields), sep = "\n")
    }
    
    return(validation_results)
    
  }, error = function(e) {
    cat("\nError:", e$message, "\n")
    return(NULL)
  })
}