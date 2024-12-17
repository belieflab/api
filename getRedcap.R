#
# function: getRedcap(instrument_name)
# input: instrument_name from table below
#

# Get full file paths of all R files in the api directory
# base::source all files using lapply()
lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)

# instrument_name                            instrument_label

# nda_study_intake                           NDA Study Intake
# demographics_overview                      Demographics + Overview
# demographics_overview_2                    Demographics 12M
# sips_p_items                               Sips P Items
# nsipr_pins                                 NSI-PR (PINS)
# sips_n_items                               Sips N Items
# sips_d_items                               Sips D Items
# sips_g_items                               Sips G Items
# family_interview_for_genetic_studies_figs  FIGS
# medication_log                             Medication Log
# life_events_scale                          Life Events Scale
# ctas                                       CTAS
# wrat                                       WRAT
# bacs                                       BACS
# hvlt                                       HVLT
# gaf_spd_criteria                           GAF + SPD Criteria
# sips_scoresheet                            SIPS Scoresheet
# gfs_social_and_role_score_sheets           GFS Social And Role Score Sheets
# scid_scoresheet                            SCID Scoresheet
# icd_scoresheet                             ICD Scoresheet
# conversion_tracker                         Conversion Tracking Form
# audit_checklist                            Audit Checklist

# variable assignment

# demographics_overview <- getRedcap("demographics_overview")
# demographics_overview_2 <- getRedcap("demographics_overview_2")
# sips_p <- getRedcap("sips_p_items")
# nsipr_pins <- getRedcap("nsipr_pins")
# sips_n <- getRedcap("sips_n_items")
# sips_d_items <- getRedcap("sips_d_items")
# sips_g_items <- getRedcap("sips_g_items")
# family_interview_for_genetic_studies_figs <- getRedcap("family_interview_for_genetic_studies_figs")
# medication_log <- getRedcap("medication_log")
# lec <- getRedcap("life_events_scale")
# ctas <- getRedcap("ctas")
# wrat <- getRedcap("wrat")
# bacs <- getRedcap("bacs")
# hvlt <- getRedcap("hvlt")
# gaf_spd_criteria <- getRedcap("gaf_spd_criteria")
# sips_scoresheet <- getRedcap("sips_scoresheet")
# gfs_social_and_role_score_sheets <- getRedcap("gfs_social_and_role_score_sheets")
# scid_scoresheet <- getRedcap("scid_scoresheet")
# icd_scoresheet <- getRedcap("icd_scoresheet")
# conversion_tracker <- getRedcap("conversion_tracker")
# audit_checklist <- getRedcap("audit_checklist")
# figs <- getRedcap("figs")

# Define the loading animation function
show_loading_animation <- function() {
  cat("Loading ")
  pb <- txtProgressBar(min = 0, max = 20, style = 3)

  for (i in 1:20) {
    Sys.sleep(0.1) # Simulate some computation time
    setTxtProgressBar(pb, i)
  }

  close(pb)
}

# Define the progress callback function
progress_callback <- function(count, total) {
  setTxtProgressBar(pb, count) # Update the loading animation
}

getRedcap <- function(instrument_name = NULL, raw_or_label = "raw", batch_size = 1000, records = NULL, fields = NULL) {
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
  
  show_loading_animation()
  
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
  }
  
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
