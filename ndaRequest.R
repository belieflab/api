#' Request and Validate Data Against NDA Structure
#'
#' This function takes a dataframe and validates it against the NDA data structure
#' by checking field names, types, and requirements against the NDA API.
#'
#' @param df A dataframe to validate
#' @param structure_name Character string of the NDA structure name to validate against
#' @param api_base_url Character string of the NDA API base URL (optional)
#' @return List containing validation results and any mismatches found
#'
#' @import httr
#' @import jsonlite
#' @import dplyr
#'
#' @export
ndaRequest <- function(df, structure_name, 
                       api_base_url = "https://nda.nih.gov/api/datadictionary/v2") {
  
  # Required packages
  required_packages <- c("httr", "jsonlite", "dplyr")
  for(pkg in required_packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg)
      library(pkg, character.only = TRUE)
    }
  }
  
  # Helper function to convert NDA type to R type
  nda_to_r_type <- function(nda_type) {
    switch(nda_type,
           "GUID" = "character",
           "String" = "character",
           "Integer" = "integer",
           "Float" = "numeric",
           "Date" = "Date",
           nda_type)
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
    cat("Element columns found:", paste(names(elements), collapse=", "), "\n")
    
    return(elements)
  }
  
  # Helper function to validate data types
  validate_data_type <- function(value, expected_type) {
    if (is.null(value) || length(value) == 0) return(TRUE)
    
    actual_type <- class(value)[1]
    r_type <- nda_to_r_type(expected_type)
    
    type_map <- list(
      "character" = c("character", "factor"),
      "integer" = c("integer", "numeric"),
      "numeric" = c("numeric", "double", "integer"),
      "Date" = c("Date", "POSIXct", "POSIXt", "character"),
      "logical" = c("logical")
    )
    
    valid_types <- type_map[[r_type]]
    if (is.null(valid_types)) {
      return(TRUE)  # Unknown type, assume valid
    }
    
    return(actual_type %in% valid_types)
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
      type_mismatches = list(),
      value_range_violations = list(),
      extra_columns = character(0),
      missing_columns = character(0)
    )
    
    # Check for required fields
    required_fields <- elements$name[elements$required == "Required"]
    results$required_fields <- required_fields  # Store for later use
    
    if (length(required_fields) > 0) {
      missing_required <- required_fields[!required_fields %in% names(df)]
      if (length(missing_required) > 0) {
        results$valid <- FALSE
        results$missing_required <- missing_required
      }
    }
    
    # Check column presence
    df_cols <- names(df)
    element_cols <- elements$name
    
    results$extra_columns <- setdiff(df_cols, element_cols)
    results$missing_columns <- setdiff(element_cols, df_cols)
    
    # Check data types and value ranges
    common_cols <- intersect(df_cols, element_cols)
    for (col in common_cols) {
      element <- elements[elements$name == col, ]
      
      # Type validation
      if (!validate_data_type(df[[col]], element$type)) {
        results$valid <- FALSE
        results$type_mismatches[[col]] <- list(
          expected = element$type,
          actual = class(df[[col]])[1]
        )
      }
      
      # Value range validation
      if (!is.null(element$valueRange) && !is.na(element$valueRange) && 
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
    cat("Fetching structure elements for", structure_name, "...\n")
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
    
    if (length(validation_results$type_mismatches) > 0) {
      cat("\nType Mismatches:\n")
      for (col in names(validation_results$type_mismatches)) {
        mismatch <- validation_results$type_mismatches[[col]]
        cat(sprintf("- %s: expected %s, got %s\n", 
                    col, mismatch$expected, mismatch$actual))
      }
    }
    
    if (length(validation_results$value_range_violations) > 0) {
      cat("\nValue Range Violations:\n")
      for (col in names(validation_results$value_range_violations)) {
        violation <- validation_results$value_range_violations[[col]]
        cat(sprintf("- %s: expected range %s, got values: %s\n",
                    col, violation$expected, violation$actual))
      }
    }
    
    if (length(validation_results$extra_columns) > 0) {
      cat("\nExtra Columns:\n")
      cat(paste("-", validation_results$extra_columns), sep = "\n")
    }
    
    if (length(validation_results$missing_columns) > 0) {
      cat("\nMissing Columns (Required fields marked with *):\n")
      missing_cols <- validation_results$missing_columns
      required_missing <- missing_cols %in% validation_results$required_fields
      cat(paste0("-", 
                 ifelse(required_missing, "* ", "  "),
                 missing_cols), sep = "\n")
    }
    
    return(validation_results)
    
  }, error = function(e) {
    cat("\nError:", e$message, "\n")
    return(NULL)
  })
}