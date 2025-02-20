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

getRedcap <- function(instrument_name = NULL, raw_or_label = "raw", redcap_event_name = NULL, batch_size = 1000, records = NULL, fields = NULL) {
  start_time <- Sys.time()
  
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
  message("")
  
  # Get super_keys data first (without event filtering)
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
  
  # Get instrument-specific data
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
  
  # Add measure column to track source
  instrument_data$measure <- instrument_name
  
  # Filter instrument data by event if specified
  if (!is.null(redcap_event_name)) {
    instrument_data <- instrument_data[instrument_data$redcap_event_name == redcap_event_name, ]
  }
  
  # Get the common join keys (typically record_id and perhaps some other identifiers)
  join_keys <- base::setdiff(colnames(super_keys_data), "redcap_event_name")
  join_keys <- base::intersect(join_keys, colnames(instrument_data))
  
  # Merge the data
  df <- base::merge(super_keys_data, instrument_data, by = join_keys, all.y = TRUE)
  
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
  
  # Calculate duration
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  message(sprintf("\nData retrieval completed in %s.", formatDuration(duration)))
  
  return(df)
}



getForms <- function() {
  if (!require(REDCapR)) install.packages("REDCapR"); library(REDCapR)
  if (!require(config)) install.packages("config"); library(config)
  if (!require(knitr)) install.packages("knitr"); library(knitr)
  if (!file.exists("secrets.R")) message("secrets.R file not found, please create it and add uri, token")
  base::source("secrets.R")
  
  forms <- REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = FALSE)$data
  
  # Option 1: Using knitr::kable for a clean table
  return(knitr::kable(forms, format = "simple"))
}

getDictionary <- function(instrument_name) {
  if (!require(REDCapR)) install.packages("REDCapR"); library(REDCapR)
  if (!require(config)) install.packages("config"); library(config)
  # check to see if secrets.R exists; if it does not, create it
  if (!file.exists("secrets.R")) message("secrets.R file not found, please create it and add uri, token")
  base::source("secrets.R") # sensitive info for api key
  metadata <- REDCapR::redcap_metadata_read(redcap_uri = uri, token = token, verbose = TRUE, config_options = NULL)$data
  dictionary <- metadata[metadata$form_name == instrument_name, ]
  # View(dictionary)
  return(dictionary)
}
