#
# function: getRedcap(instrument_name)
# input: instrument_name from table below
#

# Get full file paths of all R files in the api directory
# base::source all files using lapply()
lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)

# Initialize functions needed for the progress bar
#' @noRd
initializeLoadingAnimation <- function(steps) {
  # Get console width
  width <- tryCatch({
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::console_width() - 10  # Leave some margin
    } else {
      getOption("width", 80) - 10  # Fallback to R's width setting
    }
  }, error = function(e) 80)  # Default if all else fails
  
  list(
    steps = steps,
    current = 0,
    width = width,
    start_time = Sys.time()
  )
}

#' @noRd
updateLoadingAnimation <- function(pb, current) {
  pb$current <- current
  percentage <- round(current / pb$steps * 100)
  filled <- round(pb$width * current / pb$steps)
  bar <- paste0(
    strrep("=", filled),
    strrep(" ", pb$width - filled)
  )
  cat(sprintf("\r|%s| %3d%%", bar, percentage))  # Removed extra spaces before bar
  utils::flush.console()
}

#' @noRd
completeLoadingAnimation <- function(pb) {
  updateLoadingAnimation(pb, pb$steps)
  cat("\n")
}

#' Format a time duration in a human-readable way
#' 
#' @name formatDuration
#' @param duration The duration to format in seconds or minutes
#' @return A formatted string representing the duration
#' @noRd 
formatDuration <- function(duration) {
  secs <- as.numeric(duration, units = "secs")
  if (secs < 60) {
    return(sprintf("%.1f seconds", secs))
  } else {
    mins <- floor(secs / 60)
    remaining_secs <- round(secs %% 60, 1)
    if (remaining_secs > 0) {
      return(sprintf("%d minutes and %.1f seconds", mins, remaining_secs))
    } else {
      return(sprintf("%d minutes", mins))
    }
  }
}

#' Get Data from REDCap
#'
#' Retrieves data from a REDCap instrument and ensures subject identifiers
#' are propagated across all events
#'
#' @param instrument_name Name of the REDCap instrument
#' @param raw_or_label Whether to return raw or labeled values
#' @param redcap_event_name Optional event name filter
#' @param batch_size Number of records to retrieve per batch
#' @param records Optional vector of specific record IDs
#' @param fields Optional vector of specific fields
#'
#' @return A data frame containing the requested REDCap data
#' @export
getRedcap <- function(instrument_name = NULL, raw_or_label = "raw",
                      redcap_event_name = NULL, batch_size = 1000,
                      records = NULL, fields = NULL) {
  
  start_time <- Sys.time()
  
  #if (!require(config)) install.packages("config"); library(config)
  #if (!require(REDCapR)) install.packages("REDCapR"); library(REDCapR)
  #if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
  
  # Validate secrets and config
  base::source("api/SecretsEnv.R")
  validate_secrets("redcap")
  
  base::source("api/ConfigEnv.R")
  config <- validate_config("redcap")
  
  # Input validation
  if (is.null(instrument_name)) {
    forms_data <- REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = FALSE)$data
    forms_filtered <- forms_data[!grepl("nda", forms_data$instrument_name), ]
    random_instrument <- sample(forms_filtered$instrument_name, 1)
    forms_table <- paste(capture.output(print(getRedcapForms())), collapse = "\n")
    example_text <- sprintf("\n\nExample:\n%s <- getRedcap(\"%s\")", random_instrument, random_instrument)
    stop(sprintf("No REDCap Instrument Name provided!\n%s%s",
                 forms_table, example_text),
         call. = FALSE)
  }
  
  # Progress bar
  pb <- initializeLoadingAnimation(20)
  message(sprintf("\nImporting records from REDCap form: %s", instrument_name))
  for (i in 1:20) {
    updateLoadingAnimation(pb, i)
    Sys.sleep(0.1)
  }
  completeLoadingAnimation(pb)
  message("")
  
  # Get both instrument data and super keys data separately
  instrument_data <- REDCapR::redcap_read(
    redcap_uri = uri,
    token = token,
    forms = instrument_name,
    batch_size = batch_size,
    records = records,
    fields = fields,
    raw_or_label = raw_or_label,
    raw_or_label_headers = "raw",
    verbose = TRUE
  )$data
  
  # Get super keys data if the super keys form is defined
  if (!is.null(config$redcap$super_keys)) {
    super_keys_data <- REDCapR::redcap_read(
      redcap_uri = uri,
      token = token,
      forms = config$redcap$super_keys,
      batch_size = batch_size,
      records = records,
      raw_or_label = raw_or_label,
      raw_or_label_headers = "raw",
      verbose = TRUE
    )$data
    
    # Only process super keys if we have both datasets
    if (nrow(instrument_data) > 0 && nrow(super_keys_data) > 0) {
      # Merge instrument data with super keys
      df <- base::merge(instrument_data, super_keys_data, 
                  by = c("record_id", "redcap_event_name"), 
                  all.x = TRUE)
      
      # Now propagate any super key values across events
      if ("record_id" %in% names(df)) {
        # Find all possible super key fields
        super_key_cols <- setdiff(names(super_keys_data), 
                                  c("record_id", "redcap_event_name", 
                                    "redcap_repeat_instrument", "redcap_repeat_instance"))
        
        # Keep only fields that exist in our merged data
        super_key_cols <- super_key_cols[super_key_cols %in% names(df)]
        
        if (length(super_key_cols) > 0) {
          message("Propagating super keys across all events for each subject...")
          
          # For each subject
          for (subject_id in unique(df$record_id)) {
            # Get all rows for this subject
            subject_rows <- which(df$record_id == subject_id)
            
            # For each super key field, propagate non-NA values
            for (key_field in super_key_cols) {
              key_values <- df[subject_rows, key_field]
              non_na_values <- key_values[!is.na(key_values)]
              
              if (length(non_na_values) > 0) {
                # Use the first non-NA value for all events of this subject
                df[subject_rows, key_field] <- non_na_values[1]
              }
            }
          }
        }
      }
    } else {
      # If either dataset is empty, just use the instrument data
      df <- instrument_data
    }
  } else {
    # If no super keys form defined, just use the instrument data
    df <- instrument_data
  }
  
  # For interview_age columns
  age_cols <- grep("_interview_age$", base::names(df))
  if (length(age_cols) > 0) {
    base::names(df)[age_cols] <- "interview_age"
  }
  
  # For interview_date columns - more robust handling
  date_patterns <- c("_interview_date$", "interview_date") 
  date_cols <- NULL
  
  for (pattern in date_patterns) {
    found_cols <- grep(pattern, base::names(df), ignore.case = TRUE)
    if (length(found_cols) > 0) {
      date_cols <- found_cols
      break  # Stop at first pattern that finds matches
    }
  }
  
  if (!is.null(date_cols) && length(date_cols) > 0) {
    base::names(df)[date_cols] <- "interview_date"
  }
  
  # Apply redcap_event_name filter if specified
  if (!is.null(redcap_event_name)) {
    if (!"redcap_event_name" %in% names(df)) {
      stop("Cannot filter by redcap_event_name: column not found in data")
    }
    df <- df[df$redcap_event_name == redcap_event_name, ]
  }
  
  # Study-specific processing
  if (config$study_alias == "impact-mh") {
    if ("dob" %in% colnames(df)) {
      df <- subset(df, select = -dob)
    }
  }
  
  if (config$study_alias == "capr") {
    base::source("api/redcap/capr-logic.R")
    df <- processCaprData(df, instrument_name)
  }
  
  # Show duration
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  message(sprintf("\nData frame '%s' retrieved in %s.", instrument_name, formatDuration(duration)))
  return(df)
  # comment into add prefixes (will break code)
  #return(addPrefixToColumnss(df,instrument_name))
}


#' Get Available REDCap Forms
#' 
#' Retrieves a list of all available REDCap forms as a formatted table
#' 
#' @return A formatted table (kable) of available REDCap instruments/forms
#' @importFrom REDCapR redcap_instruments
#' @importFrom knitr kable
#' @export
getRedcapForms <- function() {
  if (!require(REDCapR)) install.packages("REDCapR"); library(REDCapR)
  if (!require(knitr)) install.packages("knitr"); library(knitr)

  # Validate secrets
  base::source("api/SecretsEnv.R")
  validate_secrets("redcap")
  
  forms <- REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = FALSE)$data
  
  # Option 1: Using knitr::kable for a clean table
  return(knitr::kable(forms, format = "simple"))
}

#' Get REDCap Data Dictionary for an Instrument
#' 
#' Retrieves the data dictionary (metadata) for a specific REDCap instrument
#' 
#' @param instrument_name Name of the REDCap instrument to retrieve dictionary for
#' @return A data frame containing the data dictionary/metadata for the specified instrument
#' @importFrom REDCapR redcap_metadata_read
#' @export
getRedcapDictionary <- function(instrument_name) {
  if (!require(REDCapR)) install.packages("REDCapR"); library(REDCapR)
  # Validate secrets
  base::source("api/SecretsEnv.R")
  validate_secrets("redcap")
  
  metadata <- REDCapR::redcap_metadata_read(redcap_uri = uri, token = token, verbose = TRUE, config_options = NULL)$data
  dictionary <- metadata[metadata$form_name == instrument_name, ]
  # View(dictionary)
  return(dictionary)
}
