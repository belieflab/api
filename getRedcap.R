#
# function: getRedcap(instrument_name)
# input: instrument_name from table below
#

# Get full file paths of all R files in the api directory
# base::source all files using lapply()
lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)

# Initialize functions needed for the progress bar
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

completeLoadingAnimation <- function(pb) {
  updateLoadingAnimation(pb, pb$steps)
  cat("\n")
}

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

getRedcap <- function(instrument_name = NULL, raw_or_label = "raw", batch_size = 1000, records = NULL, fields = NULL) {
  start_time <- Sys.time()  # Add this at the start
  
  if (!require(config)) install.packages("config"); library(config)
  if (!require(REDCapR)) install.packages("REDCapR"); library(REDCapR)
  if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
  
  # If instrument_name parameter is NULL, show warning:
  if (is.null(instrument_name)) {
    forms_data <- REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = FALSE)$data
    # Filter out nda form before sampling
    forms_filtered <- forms_data[!grepl("nda", forms_data$instrument_name), ]
    random_instrument <- sample(forms_filtered$instrument_name, 1)
    
    forms_table <- paste(capture.output(print(getForms())), collapse = "\n")
    example_text <- sprintf("\n\nExample:\n%s <- getRedcap(\"%s\")", random_instrument, random_instrument)
    
    # First display the error
    stop(sprintf("No REDCap Instrument Name provided!\n%s%s", 
                 forms_table,
                 example_text), 
         call. = FALSE)
  }
  
  config <- config::get()
  
  if (!file.exists("secrets.R")) {
    stop("secrets.R file not found, please create it and add uri, token")
  }
  
  base::source("secrets.R")
  
  # Initialize progress bar
  pb <- initializeLoadingAnimation(20)
  message(sprintf("\nImporting records from REDCap form: %s", instrument_name))
  
  # Update progress before data retrieval
  for (i in 1:20) {
    updateLoadingAnimation(pb, i)
    Sys.sleep(0.1)
  }
  completeLoadingAnimation(pb)
  message("")  # Add blank line before REDCap messages
  
  
  # Get data based on whether specific fields were provided
  if (!is.null(fields)) {
    df <- REDCapR::redcap_read(
      redcap_uri = uri,
      token = token,
      fields = fields,
      forms = c(config$redcap$super_keys,instrument_name),
      batch_size = batch_size,
      records = records,
      raw_or_label = raw_or_label,    # Added this parameter
      raw_or_label_headers = raw_or_label,
      # guess_type = FALSE,      # Prevent type guessing issues
      verbose = TRUE
    )$data
  } else {
    df <- REDCapR::redcap_read(
      redcap_uri = uri,
      token = token,
      forms = c(config$redcap$super_keys,instrument_name),
      batch_size = batch_size,
      records = records,
      raw_or_label = raw_or_label,    # Added this parameter
      raw_or_label_headers = raw_or_label,
      # guess_type = FALSE,      # Prevent type guessing issues
      verbose = TRUE
    )$data
  }
  
  # Add measure column to track source
  df$measure <- instrument_name
  
  # Return the raw dataframe if not CAPR study
  if (config$study_alias == "impact-mh") {
    df <- subset(df, select = -dob)
  }
  
  # CAPR-specific processing only happens if study_alias is "capr"
  if (config$study_alias == "capr") {
    base::source("api/redcap/capr-logic.R")
    df <- processCaprData(df, instrument_name)  # New function to process CAPR data
  }
  
  # At the end, add duration message without repeating progress bar
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  message(sprintf("\nData retrieval completed in %s.", formatDuration(duration)))
  
  return(df)
}

getForms <- function() {
  if (!require(knitr)) install.packages("knitr"); library(knitr)
  if (!file.exists("secrets.R")) message("secrets.R file not found, please create it and add uri, token")
  base::source("secrets.R")
  
  forms <- REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = FALSE)$data
  
  # Option 1: Using knitr::kable for a clean table
  return(knitr::kable(forms, format = "simple"))
}

getDictionary <- function(instrument_name) {
  # check to see if secrets.R exists; if it does not, create it
  if (!file.exists("secrets.R")) message("secrets.R file not found, please create it and add uri, token")
  base::source("secrets.R") # sensitive info for api key
  metadata <- REDCapR::redcap_metadata_read(redcap_uri = uri, token = token, verbose = TRUE, config_options = NULL)$data
  dictionary <- metadata[metadata$form_name == instrument_name, ]
  # View(dictionary)
  return(dictionary)
}
