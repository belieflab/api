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

# todo: check type field (integers. floats)


# Helper function for EEFRT specific transformations
transform_eefrt_values <- function(df, measure_name) {
  if (measure_name == "eefrt") {
    cat("\nApplying EEFRT-specific transformations...")
    
    # Rename 'index' to 'trial' if it exists, only for positive values
    if ("index" %in% names(df)) {
      # Convert index to numeric if it's not already
      df$index <- as.numeric(df$index)
      
      # Create trial column maintaining the same length as the dataframe
      df$trial <- df$index
      # Set non-positive values to NA or appropriate missing value
      df$trial[df$index <= 0] <- NA
      
      # Count the transformations
      total_rows <- length(df$index)
      positive_rows <- sum(df$index > 0, na.rm = TRUE)
      
      # Remove the original index column
      df$index <- NULL
      
      cat(sprintf("\nRenamed 'index' to 'trial' for EEFRT task (kept %d positive values out of %d total rows)", 
                  positive_rows, total_rows))
    }
    
    # Transform reward_hard decimals if field exists
    if ("reward_hard" %in% names(df)) {
      # Convert to numeric first in case it's character
      df$reward_hard <- as.numeric(df$reward_hard)
      
      # Store original values for logging
      original_values <- head(df$reward_hard)
      
      # Function to check if a number has decimals
      has_decimals <- function(x) {
        !is.na(x) && abs(x - floor(x)) > 0
      }
      
      # Only transform values that have decimals
      decimal_mask <- sapply(df$reward_hard, has_decimals)
      if(any(decimal_mask)) {
        df$reward_hard[decimal_mask] <- round(df$reward_hard[decimal_mask] * 100)
        
        cat("\nTransformed decimal reward_hard values only")
        cat("\nBefore:", paste(original_values[1:3], collapse=", "))
        cat("\nAfter:", paste(head(df$reward_hard)[1:3], collapse=", "))
        cat(sprintf("\nModified %d decimal values out of %d total values", 
                    sum(decimal_mask), length(decimal_mask)))
      } else {
        cat("\nNo decimal values found in reward_hard - no transformations needed")
      }
    }
    
    cat("\nEEFRT transformations complete\n")
  }
  return(df)
}

# Extract mapping rules from Notes field
# Modified get_mapping_rules function with better error handling
get_mapping_rules <- function(notes) {
  if (is.null(notes) || is.na(notes) || notes == "") return(NULL)
  
  rules <- list()
  
  tryCatch({
    # Handle array notation like "1=(0.9, 0.5, 0.1)"
    if (grepl("=\\(.*\\)", notes)) {
      pattern_matches <- gregexpr("(\\d+)=\\(([^)]+)\\)", notes)
      if (pattern_matches[[1]][1] != -1) {
        matches <- regmatches(notes, pattern_matches)[[1]]
        for (match in matches) {
          code_match <- regexec("(\\d+)=\\(([^)]+)\\)", match)
          parts <- regmatches(match, code_match)[[1]]
          if (length(parts) >= 3) {  # Check if we have enough parts
            code <- parts[2]
            values <- sprintf("[%s]", parts[3])
            rules[[values]] <- code
          }
        }
      }
    }
    
    # Handle simple mappings like "1=Red" and "NaN=-1"
    if (grepl("[^=]+=[^;]+", notes)) {
      patterns <- strsplit(notes, ";\\s*")[[1]]
      for (pattern in patterns) {
        if (grepl("=", pattern)) {
          parts <- strsplit(pattern, "=")[[1]]
          if (length(parts) >= 2) {  # Check if we have both parts
            value <- trimws(parts[1])
            code <- trimws(parts[2])
            rules[[code]] <- value
          }
        }
      }
    }
  }, error = function(e) {
    warning(sprintf("Error parsing mapping rules: %s\nNotes: %s", e$message, notes))
    return(list())  # Return empty list on error instead of NULL
  })
  
  return(rules)
}

# semi-generalizable e.g. handle mooney rt null
apply_null_transformations <- function(df, elements) {
  for (i in 1:nrow(elements)) {
    field_name <- elements$name[i]
    type <- elements$type[i]
    notes <- elements$notes[i]
    
    if (field_name %in% names(df) && !is.null(notes)) {
      # Extract transformation rules from Notes
      rules <- get_mapping_rules(notes)
      
      if (!is.null(rules) && length(rules) > 0) {
        cat(sprintf("\nRules for field '%s':\n", field_name))
        print(rules)
        
        null_placeholder <- as.numeric(rules[[1]])
        
        cat(sprintf("Using placeholder value: %s\n", null_placeholder))
        
        # Add debugging before conversion
        cat(sprintf("\nUnique values before conversion in %s:\n", field_name))
        print(unique(df[[field_name]]))
        
        message("applying type conversions")
        
        df[[field_name]] <- as.character(df[[field_name]])
        
        null_mask <- df[[field_name]] %in% c("null", "NaN", "") | is.na(df[[field_name]])
        df[[field_name]][null_mask] <- null_placeholder
        
        # Add debugging for type conversion
        if (type == "Integer" || type == "Float") {
          cat(sprintf("\nConverting %s to %s\n", field_name, type))
          # Check for problematic values before conversion
          non_numeric <- df[[field_name]][!grepl("^-?\\d*\\.?\\d+$", df[[field_name]])]
          if (length(non_numeric) > 0) {
            cat(sprintf("Warning: Non-numeric values found in %s:\n", field_name))
            print(unique(non_numeric))
          }
          
          if (type == "Integer") {
            df[[field_name]] <- as.integer(df[[field_name]])
          } else if (type == "Float") {
            df[[field_name]] <- as.numeric(df[[field_name]])
          }
          
          # Check for NAs after conversion
          new_nas <- is.na(df[[field_name]])
          if (any(new_nas)) {
            cat(sprintf("\nWarning: %d NAs introduced in %s\n", sum(new_nas), field_name))
            cat("Sample of values that became NA:\n")
            print(head(df[[field_name]][new_nas]))
          }
        }
        
        cat(sprintf("Values after transformation: %s\n", 
                    paste(unique(df[[field_name]]), collapse = ", ")))
      }
    }
  }
  return(df)
}



# Convert fields to their proper type based on NDA definition
# Modify the apply_type_conversions function to be more robust
apply_type_conversions <- function(df, elements) {
  for (i in 1:nrow(elements)) {
    field_name <- elements$name[i]
    type <- elements$type[i]
    
    if (field_name %in% names(df) && !is.null(type)) {
      tryCatch({
        # Handle numeric types
        if (type %in% c("Integer", "Float")) {
          # First convert to character to handle any unexpected formats
          df[[field_name]] <- as.character(df[[field_name]])
          
          # Remove any currency symbols, commas, etc
          df[[field_name]] <- gsub("[^0-9.-]", "", df[[field_name]])
          
          if (type == "Integer") {
            # Convert to numeric first to handle decimals
            df[[field_name]] <- as.numeric(df[[field_name]])
            
            float_mask <- !is.na(df[[field_name]]) & 
              abs(df[[field_name]] - floor(df[[field_name]])) > 0
            
            if (any(float_mask)) {
              float_examples <- head(df[[field_name]][float_mask])
              rounded_examples <- round(float_examples)
              cat(sprintf("\nRounding float values in %s to integers\n", field_name))
              cat("Example conversions:\n")
              for (i in seq_along(float_examples)) {
                cat(sprintf("  %.2f -> %d\n", float_examples[i], rounded_examples[i]))
              }
              cat(sprintf("Total float values rounded: %d\n", sum(float_mask)))
              df[[field_name]] <- round(df[[field_name]])
            }
            
            df[[field_name]] <- as.integer(df[[field_name]])
          }
          else if (type == "Float") {
            df[[field_name]] <- as.numeric(df[[field_name]])
          }
          
          # Report any NAs introduced
          na_count <- sum(is.na(df[[field_name]]))
          if (na_count > 0) {
            cat(sprintf("\nWarning: %d NA values in %s after type conversion\n", 
                        na_count, field_name))
          }
        }
      }, error = function(e) {
        warning(sprintf("Error converting %s to %s: %s", field_name, type, e$message))
      })
    }
  }
  return(df)
}

 
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

# Parse array-like strings to vectors
parse_array_string <- function(value) {
  if (is.null(value) || is.na(value)) return(NULL)
  
  # Handle string arrays
  if (is.character(value)) {
    # Remove unicode prefix, brackets, and quotes
    clean_str <- gsub("\\[|\\]|u'|'", "", value)
    values <- strsplit(clean_str, ",\\s*")[[1]]
    return(tolower(trimws(values)))
  }
  
  # Handle numeric arrays
  if (is.numeric(value) && length(value) > 1) {
    return(sprintf("%.1f", value))
  }
  
  return(tolower(trimws(value)))
}

# Transform values based on NDA structure requirements
# Add case mapping standardization to transform_value_ranges (e.g. eefrt01 e, h)
transform_value_ranges <- function(df, elements) {
  # Check which columns are required
  required_fields <- elements$name[elements$required == "Required"]
  
  # Only check non-required columns for emptiness e.g."rt" on eefrt
  empty_cols <- sapply(df[, !names(df) %in% required_fields], function(col) all(is.na(col) | col == ""))
  if (any(empty_cols)) {
    df <- df[, !names(df) %in% names(empty_cols)[empty_cols], drop=FALSE]
    cat("\nDropped empty columns:", paste(names(empty_cols)[empty_cols], collapse=", "), "\n")
  }
  
  # Keep existing binary field transformation
  binary_fields <- elements$name[!is.na(elements$valueRange) & elements$valueRange == "0;1"]
  if (length(binary_fields) > 0) {
    for (field in binary_fields) {
      if (field %in% names(df)) {
        values <- as.character(df[[field]])
        potential_booleans <- c("true", "false", "t", "f", "TRUE", "FALSE", "True", "False")
        if (any(values %in% potential_booleans, na.rm = TRUE)) {
          cat(sprintf("\nChecking %s format (0;1)...", field))
          df[[field]] <- standardize_binary(df[[field]])
        }
      }
    }
  }
  
  # Process fields that have value range rules
  for (i in 1:nrow(elements)) {
    field_name <- elements$name[i]
    value_range <- elements$valueRange[i]
    
    if (field_name %in% names(df) && !is.na(value_range) && grepl(";", value_range)) {
      expected_values <- trimws(unlist(strsplit(value_range, ";")))
      current_values <- df[[field_name]]
      
      # Map case-insensitive matches to expected case
      for (exp_val in expected_values) {
        matches <- tolower(current_values) == tolower(exp_val)
        if (any(matches)) {
          df[[field_name]][matches] <- exp_val
        }
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

# Main validation logic function
# Modified validate_structure function with better error handling
validate_structure <- function(df, elements, measure_name) {
  results <- list(
    valid = TRUE,
    missing_required = character(0),
    value_range_violations = list(),
    unknown_fields = character(0),
    warnings = character(0)  # New field for tracking warnings
  )
  
  tryCatch({
    # Get required fields and all valid fields
    required_fields <- elements$name[elements$required == "Required"]
    valid_fields <- elements$name
    
    # Check for unknown fields
    df_cols <- names(df)
    results$unknown_fields <- setdiff(df_cols, valid_fields)
    if (length(results$unknown_fields) > 0) {
      results$valid <- FALSE
      results$warnings <- c(results$warnings, 
                            sprintf("Found %d unknown fields", length(results$unknown_fields)))
    }
    
    # Check for required fields with more detailed logging
    if (length(required_fields) > 0) {
      missing_required <- required_fields[!required_fields %in% df_cols]
      if (length(missing_required) > 0) {
        results$valid <- FALSE
        results$missing_required <- missing_required
        results$warnings <- c(results$warnings, 
                              sprintf("Missing %d required fields: %s", 
                                      length(missing_required),
                                      paste(missing_required, collapse=", ")))
      }
    }
    
    # Check value ranges with better error handling
    for (col in intersect(df_cols, valid_fields)) {
      element <- elements[elements$name == col, ]
      
      if (nrow(element) > 0 && !is.null(element$valueRange) && 
          !is.na(element$valueRange) && element$valueRange != "") {
        
        tryCatch({
          # If it's a 0;1 range field, standardize boolean values first
          if (element$valueRange == "0;1") {
            values <- as.character(df[[col]])
            if (any(tolower(values) %in% c("true", "false"))) {
              cat(sprintf("\nStandardizing boolean values in %s...\n", col))
              df[[col]] <- standardize_binary(values)
              assign(measure_name, df, envir = .GlobalEnv)
            }
          }
          
          # Get the violating values with error handling
          violating_values <- tryCatch({
            get_violations(df[[col]], element$valueRange)
          }, error = function(e) {
            results$warnings <- c(results$warnings,
                                  sprintf("Error checking violations for %s: %s", 
                                          col, e$message))
            return(character(0))
          })
          
          if (length(violating_values) > 0) {
            results$valid <- FALSE
            results$value_range_violations[[col]] <- list(
              expected = element$valueRange,
              actual = paste(violating_values, collapse = ", ")
            )
          }
        }, error = function(e) {
          results$warnings <- c(results$warnings,
                                sprintf("Error processing column %s: %s", col, e$message))
        })
      }
    }
    
  }, error = function(e) {
    results$valid <- FALSE
    results$warnings <- c(results$warnings,
                          sprintf("Critical validation error: %s", e$message))
  })
  
  return(results)
}

# Add this function to handle date standardization
standardize_dates <- function(df, date_cols = c("interview_date")) {
  for (col in date_cols) {
    if (col %in% names(df)) {
      tryCatch({
        # First try to identify the date format
        dates <- df[[col]]
        
        # Remove any timezone information
        dates <- gsub("\\s+\\d{2}:\\d{2}:\\d{2}.*$", "", dates)
        
        # Try different date formats
        parsed_dates <- NULL
        date_formats <- c(
          "%Y-%m-%d",    # 2023-12-31
          "%m/%d/%Y",    # 12/31/2023
          "%Y/%m/%d",    # 2023/12/31
          "%d-%m-%Y",    # 31-12-2023
          "%m-%d-%Y"     # 12-31-2023
        )
        
        for (format in date_formats) {
          parsed_dates <- tryCatch({
            as.Date(dates, format = format)
          }, error = function(e) NULL)
          
          if (!is.null(parsed_dates) && !all(is.na(parsed_dates))) {
            break  # Found a working format
          }
        }
        
        if (is.null(parsed_dates) || all(is.na(parsed_dates))) {
          warning(sprintf("Could not parse dates in column %s. Sample values: %s", 
                          col, paste(head(dates), collapse=", ")))
          next
        }
        
        # Convert to YYYY-MM-DD format
        df[[col]] <- format(parsed_dates, "%Y-%m-%d")
        
        cat(sprintf("\nStandardized dates in column %s to YYYY-MM-DD format\n", col))
        cat("Sample values:", paste(head(df[[col]]), collapse=", "), "\n")
        
      }, error = function(e) {
        warning(sprintf("Error processing dates in column %s: %s", col, e$message))
      })
    }
  }
  return(df)
}

# Modify the main validation function to include date standardization
# Add enhanced debug logging
debug_print <- function(msg, df = NULL, sample_size = 5) {
  cat("\nDEBUG:", msg, "\n")
  if (!is.null(df)) {
    cat("Dataframe info:\n")
    cat("- Dimensions:", paste(dim(df), collapse=" x "), "\n")
    cat("- Column names:", paste(names(df), collapse=", "), "\n")
    cat("- First", sample_size, "rows of data:\n")
    print(head(df, sample_size))
  }
}

# Modified ndaValidator with enhanced error handling
# Helper function to standardize column names
standardize_column_names <- function(df, structure_name) {
  # Get structure prefix (e.g., "ahrs" from "ahrs01")
  prefix <- sub("01$", "", structure_name)
  
  # Create name mapping function
  standardize_name <- function(name) {
    # Convert to lowercase
    name <- tolower(name)
    # Replace hyphens with underscores
    name <- gsub("-", "_", name)
    # Handle prefix if present
    if (grepl(paste0("^", prefix, "[_-]?\\d+$"), name)) {
      # Ensure consistent underscore between prefix and number
      name <- gsub(paste0("^(", prefix, ")[_-]?(\\d+)$"), "\\1_\\2", name)
    }
    return(name)
  }
  
  # Standardize column names
  old_names <- names(df)
  new_names <- sapply(old_names, standardize_name)
  
  # Report changes
  changed <- old_names != new_names
  if (any(changed)) {
    cat("\nStandardizing column names:")
    for (i in which(changed)) {
      cat(sprintf("\n  %s -> %s", old_names[i], new_names[i]))
    }
    cat("\n")
  }
  
  # Apply new names
  names(df) <- new_names
  return(df)
}

# Modify transform_value_ranges to be more robust
transform_value_ranges <- function(df, elements) {
  tryCatch({
    # Check which columns are required
    required_fields <- elements$name[elements$required == "Required"]
    
    # Only check non-required columns for emptiness
    empty_cols <- sapply(df[, !names(df) %in% required_fields], function(col) {
      all(is.na(col) | col == "")
    })
    if (any(empty_cols)) {
      df <- df[, !names(df) %in% names(empty_cols)[empty_cols], drop=FALSE]
      cat("\nDropped empty columns:", paste(names(empty_cols)[empty_cols], collapse=", "), "\n")
    }
    
    # Process fields that have value range rules
    for (i in 1:nrow(elements)) {
      field_name <- elements$name[i]
      value_range <- elements$valueRange[i]
      
      if (field_name %in% names(df) && !is.na(value_range) && value_range != "") {
        cat(sprintf("\nProcessing value range for %s: %s\n", field_name, value_range))
        
        # Handle binary fields (0;1)
        if (value_range == "0;1") {
          values <- as.character(df[[field_name]])
          if (any(tolower(values) %in% c("true", "false", "t", "f"))) {
            df[[field_name]] <- standardize_binary(values)
          }
        }
        
        # Handle other value ranges
        else if (grepl(";", value_range)) {
          expected_values <- trimws(unlist(strsplit(value_range, ";")))
          current_values <- df[[field_name]]
          
          # Map case-insensitive matches to expected case
          for (exp_val in expected_values) {
            matches <- tolower(current_values) == tolower(exp_val)
            if (any(matches)) {
              df[[field_name]][matches] <- exp_val
            }
          }
        }
      }
    }
    
    return(df)
    
  }, error = function(e) {
    cat("\nError in transform_value_ranges:", e$message, "\n")
    return(df)  # Return unchanged dataframe on error
  })
}

# Modified ndaValidator to include column name standardization
ndaValidator <- function(measure_name, source,
                         api_base_url = "https://nda.nih.gov/api/datadictionary/v2") {
  
  tryCatch({
    # Get the dataframe from the global environment
    df <- base::get(measure_name, envir = .GlobalEnv)
    debug_print("Initial dataframe loaded", df)
    
    # Add explicit date standardization step
    df <- standardize_dates(df)
    debug_print("After date standardization", df)
    
    # Get structure name
    structure_name <- paste0(measure_name, "01")
    
    # Standardize column names based on structure
    df <- standardize_column_names(df, structure_name)
    debug_print("After column name standardization", df)
    
    # Save standardized dataframe back to global environment
    assign(measure_name, df, envir = .GlobalEnv)
    
    # Continue with structure fetching and validation...
    message("\nFetching ", structure_name, " Data Structure from NDA API...")
    elements <- fetch_structure_elements(structure_name, api_base_url)
    
    if (is.null(elements) || nrow(elements) == 0) {
      stop("No elements found in the structure definition")
    }
    
    # Process the dataframe with additional error handling
    df <- tryCatch({
      # Apply type conversions
      df <- apply_type_conversions(df, elements)
      
      # Apply null transformations
      df <- apply_null_transformations(df, elements)
      
      # Transform value ranges
      df <- transform_value_ranges(df, elements)
      
      df
    }, error = function(e) {
      stop(sprintf("Error processing dataframe: %s", e$message))
    })
    
    # Save processed dataframe back to global environment
    assign(measure_name, df, envir = .GlobalEnv)
    
    # Validate structure
    validation_results <- validate_structure(df, elements, measure_name)
    
    return(validation_results)
    
  }, error = function(e) {
    message("Error: ", e$message)
    return(NULL)
  })
}

# Modified apply_null_transformations with better error handling
apply_null_transformations <- function(df, elements) {
  for (i in 1:nrow(elements)) {
    field_name <- elements$name[i]
    type <- elements$type[i]
    notes <- elements$notes[i]
    
    if (field_name %in% names(df) && !is.null(notes)) {
      tryCatch({
        cat(sprintf("\nProcessing field: %s\n", field_name))
        
        # Extract transformation rules from Notes
        rules <- get_mapping_rules(notes)
        
        if (!is.null(rules) && length(rules) > 0) {
          cat(sprintf("Rules found for field '%s':\n", field_name))
          print(rules)
          
          # Get placeholder value safely
          null_placeholder <- tryCatch({
            as.numeric(rules[[1]])
          }, error = function(e) {
            warning(sprintf("Could not convert placeholder to numeric for %s: %s", 
                            field_name, e$message))
            NA
          })
          
          cat(sprintf("Using placeholder value: %s\n", 
                      if(is.na(null_placeholder)) "NA" else null_placeholder))
          
          # Show current values
          cat(sprintf("Unique values before conversion in %s:\n", field_name))
          print(unique(df[[field_name]]))
          
          # Convert field to character first
          df[[field_name]] <- as.character(df[[field_name]])
          
          # Apply null transformations
          null_mask <- df[[field_name]] %in% c("null", "NaN", "") | is.na(df[[field_name]])
          df[[field_name]][null_mask] <- null_placeholder
          
          # Apply type conversion if needed
          if (type %in% c("Integer", "Float")) {
            cat(sprintf("Converting %s to %s\n", field_name, type))
            if (type == "Integer") {
              df[[field_name]] <- as.integer(df[[field_name]])
            } else {
              df[[field_name]] <- as.numeric(df[[field_name]])
            }
          }
          
          cat(sprintf("Values after transformation: %s\n",
                      paste(unique(df[[field_name]]), collapse = ", ")))
        }
      }, error = function(e) {
        warning(sprintf("Error processing field %s: %s", field_name, e$message))
      })
    }
  }
  return(df)
}