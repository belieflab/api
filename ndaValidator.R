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

# Calculate Levenshtein distance similarity between two strings
calculate_similarity <- function(str1, str2) {
  # Convert to lowercase
  str1 <- tolower(str1)
  str2 <- tolower(str2)
  
  # Create matrix
  m <- nchar(str1)
  n <- nchar(str2)
  d <- matrix(0, nrow = m + 1, ncol = n + 1)
  d[1,] <- 0:n
  d[,1] <- 0:m
  
  # Fill matrix
  for (i in 2:(m + 1)) {
    for (j in 2:(n + 1)) {
      cost <- if (substr(str1, i-1, i-1) == substr(str2, j-1, j-1)) 0 else 1
      d[i,j] <- min(
        d[i-1,j] + 1,      # deletion 
        d[i,j-1] + 1,      # insertion
        d[i-1,j-1] + cost  # substitution
      )
    }
  }
  
  # Return similarity score (1 - normalized distance)
  return(1 - d[m+1,n+1] / max(m,n))
}

# Helper function to standardize handedness values
standardize_handedness <- function(value) {
  # Create mapping for handedness terms
  handedness_map <- c(
    "left" = "L",
    "l" = "L",
    "right" = "R",
    "r" = "R",
    "both" = "B",
    "ambidextrous" = "B"
  )
  
  # Convert to lowercase for consistent matching
  value <- tolower(value)
  
  # Map values using the handedness_map
  mapped_values <- handedness_map[value]
  mapped_values[is.na(mapped_values)] <- value[is.na(mapped_values)]
  
  # Count and report transformations
  n_transformed <- sum(value != mapped_values, na.rm = TRUE)
  if (n_transformed > 0) {
    cat(sprintf("\nTransformed %d handedness values to NDA standard format\n", n_transformed))
  }
  
  return(mapped_values)
}

# Helper function to standardize boolean to numeric values
standardize_binary <- function(value) {
  # Create mapping for boolean to numeric terms (including case variations)
  binary_map <- c(
    "true" = "1",
    "false" = "0",
    "t" = "1",
    "f" = "0",
    "TRUE" = "1",
    "FALSE" = "0",
    "True" = "1",
    "False" = "0"
  )
  
  # Convert value to character without changing case
  value <- as.character(value)
  
  # Map values using the binary_map (exact match)
  mapped_values <- binary_map[value]
  
  # For any unmatched values, try lowercase matching
  still_na <- is.na(mapped_values)
  if(any(still_na)) {
    mapped_values[still_na] <- binary_map[tolower(value[still_na])]
  }
  
  # Keep original values for any remaining unmatched
  mapped_values[is.na(mapped_values)] <- value[is.na(mapped_values)]
  
  # Count and report transformations
  n_transformed <- sum(value != mapped_values, na.rm = TRUE)
  if (n_transformed > 0) {
    cat(sprintf("\nTransformed %d boolean values to 0/1 format\n", n_transformed))
  }
  
  return(mapped_values)
}

# Transform values based on NDA structure requirements
transform_value_ranges <- function(df, elements) {
  # Transform handedness specifically
  if ("handedness" %in% names(df)) {
    cat("\nChecking handedness format...")
    df$handedness <- standardize_handedness(df$handedness)
  }
  
  # Identify fields with "0;1" valueRange
  binary_fields <- elements$name[elements$valueRange == "0;1"]
  
  # Transform any binary fields found in the data
  for (field in binary_fields) {
    if (field %in% names(df)) {
      # Check if the field contains any boolean-like values
      values <- as.character(df[[field]])
      potential_booleans <- c("true", "false", "t", "f", "TRUE", "FALSE", "True", "False")
      if (any(values %in% potential_booleans)) {
        cat(sprintf("\nChecking %s format (0;1)...", field))
        df[[field]] <- standardize_binary(df[[field]])
      }
    }
  }
  
  return(df)
}

# Helper function to fetch structure elements from API
fetch_structure_elements <- function(structure_name, api_base_url) {
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

# Find similar fields and handle renaming
find_and_rename_fields <- function(df, elements, structure_name) {
  renamed <- list(
    df = df,
    renames = character(),
    columns_to_drop = character()
  )
  
  # Get dataframe column names
  df_cols <- names(df)
  valid_fields <- elements$name
  
  # Get structure short name (e.g., "deldisk" from "deldisk01")
  structure_prefix <- sub("01$", "", structure_name)
  
  # Find unknown fields
  unknown_fields <- setdiff(df_cols, valid_fields)
  
  if (length(unknown_fields) > 0) {
    cat("\nChecking for similar field names...\n")
    
    for (field in unknown_fields) {
      # Try with structure prefix first
      prefixed_field <- paste0(structure_prefix, "_", field)
      
      # Calculate similarity with all valid fields
      similarities <- sapply(valid_fields, function(name) {
        max(
          calculate_similarity(field, name),
          calculate_similarity(prefixed_field, name)
        )
      })
      
      # Remove any NA values
      similarities <- similarities[!is.na(similarities)]
      
      if (length(similarities) > 0) {
        # Find best match if similarity > 0.7 (lowered threshold)
        best_match <- names(similarities)[which.max(similarities)]
        best_score <- max(similarities)
        
        if (best_score > 0.7 && !best_match %in% df_cols) {
          cat(sprintf("Renaming '%s' to '%s' (similarity: %.2f)\n", 
                      field, best_match, best_score))
          
          # Add the new column with renamed data
          renamed$df[[best_match]] <- renamed$df[[field]]
          
          # Mark original column for dropping
          renamed$columns_to_drop <- c(renamed$columns_to_drop, field)
          
          # Store the rename operation
          renamed$renames <- c(renamed$renames, 
                               sprintf("%s -> %s", field, best_match))
        } else {
          # Show possible matches if any are above 0.5
          top_matches <- names(sort(similarities[similarities > 0.5], decreasing = TRUE))[1:3]
          if (length(top_matches) > 0) {
            cat(sprintf("\nPossible matches for '%s':\n", field))
            for (match in top_matches) {
              cat(sprintf("  - %s (similarity: %.2f)\n", match, similarities[match]))
            }
          } else {
            cat(sprintf("\nNo close matches found for '%s'\n", field))
          }
        }
      } else {
        cat(sprintf("\nNo valid matches found for '%s'\n", field))
      }
    }
    
    # Drop original columns after all renaming is done
    if (length(renamed$columns_to_drop) > 0) {
      renamed$df <- renamed$df[, !names(renamed$df) %in% renamed$columns_to_drop]
      cat("\nDropped original columns after renaming:\n")
      cat(paste("-", renamed$columns_to_drop), sep = "\n")
    }
  }
  
  return(renamed)
}

# Helper function to get violating values with type conversion
get_violations <- function(value, range_str) {
  if (is.null(range_str) || is.na(range_str) || range_str == "") return(character(0))
  
  range_str <- gsub(" ", "", range_str)
  
  if (grepl("::", range_str)) {
    # Numeric range check
    range <- as.numeric(strsplit(range_str, "::")[[1]])
    
    # Convert value to numeric if it's character
    if (is.character(value)) {
      value <- as.numeric(value)
    }
    
    invalid_mask <- value < range[1] | value > range[2]
    invalid_mask[is.na(invalid_mask)] <- FALSE
    return(sort(unique(value[invalid_mask])))
  } else if (grepl(";", range_str)) {
    # Categorical values - already standardized by this point
    valid_values <- trimws(strsplit(range_str, ";")[[1]])
    invalid_mask <- !(as.character(value) %in% valid_values)
    invalid_mask[is.na(invalid_mask)] <- FALSE
    return(sort(unique(value[invalid_mask])))
  }
  
  return(character(0))
}

# Main validation logic function with binary standardization
validate_structure <- function(df, elements, measure_name) {  # Add measure_name parameter
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
        element$valueRange != "") {
      
      # If it's a 0;1 range field, standardize boolean values first
      if (element$valueRange == "0;1") {
        values <- as.character(df[[col]])
        if (any(tolower(values) %in% c("true", "false"))) {
          cat(sprintf("\nStandardizing boolean values in %s...\n", col))
          df[[col]] <- standardize_binary(values)
          # Update the global dataframe
          assign(measure_name, df, envir = .GlobalEnv)
        }
      }
      
      # Get the violating values
      violating_values <- get_violations(df[[col]], element$valueRange)
      
      if (length(violating_values) > 0) {
        results$valid <- FALSE
        results$value_range_violations[[col]] <- list(
          expected = element$valueRange,
          actual = paste(violating_values, collapse = ", ")
        )
      }
    }
  }
  
  return(results)
}

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
  
  # Transform handedness if present
  if ("handedness" %in% names(df)) {
    cat("\nChecking handedness format...")
    df$handedness <- standardize_handedness(df$handedness)
    # Save back to global environment after handedness transformation
    assign(measure_name, df, envir = .GlobalEnv)
  }
  
  tryCatch({
    # Fetch elements first to get structure
    cat("Fetching NDA Data Structure for", structure_name, "...\n")
    elements <- fetch_structure_elements(structure_name, api_base_url)
    if (is.null(elements) || nrow(elements) == 0) {
      stop("No elements found in the structure definition")
    }
    
    # Transform values based on structure requirements
    df <- transform_value_ranges(df, elements)
    assign(measure_name, df, envir = .GlobalEnv)
    
    # Auto-rename fields based on similarity
    renamed <- find_and_rename_fields(df, elements, structure_name)
    df <- renamed$df  # Get renamed dataframe
    
    if (length(renamed$renames) > 0) {
      cat("\nAutomatically renamed fields:\n")
      cat(paste("-", renamed$renames), sep = "\n")
      
      # Save the renamed dataframe back to the global environment
      assign(measure_name, df, envir = .GlobalEnv)
      
      # Show new column names
      cat("\nUpdated column names:\n")
      cat(paste(names(df), collapse = " "), "\n")
    }
    
    # Continue with validation using renamed dataframe
    validation_results <- validate_structure(df, elements, measure_name)  # Pass measure_name
    
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
    
    # Drop unknown fields from dataframe
    if (length(validation_results$unknown_fields) > 0) {
      cat("\nDropping unknown fields from dataframe...\n")
      df <- df[, !(names(df) %in% validation_results$unknown_fields)]
      assign(measure_name, df, envir = .GlobalEnv)

      # Show final column names
      cat("\nFinal column names:\n")
      cat(paste(names(df), collapse = " "), "\n")
    }
    
    return(validation_results)
    
  }, error = function(e) {
    cat("\nError:", e$message, "\n")
    return(NULL)
  })
}