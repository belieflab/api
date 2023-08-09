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
    Sys.sleep(0.1)  # Simulate some computation time
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
}

# Define the progress callback function
progress_callback <- function(count, total) {
  setTxtProgressBar(pb, count)  # Update the loading animation
}

getRedcap <- function(instrument_name) {
  
  if(!require(config)) {install.packages("config")}; library(config)
  
  config <- config::get()
  
  # documentation
  # https://www.richardshanna.com/tutorial/redcapapi/
  
  # installs REDCapR if not already installed; load REDCapR
  if(!require(REDCapR)) {install.packages("REDCapR")}; library(REDCapR)
  if(!require(tidyverse)) {install.packages("tidyverse")};library(tidyverse)

  # check to see if secrets.R exists; if it does not, create it
  if (!file.exists("secrets.R")) message("secrets.R file not found, please create it and add uri, token")
  
  base::source("secrets.R"); # sensitive info for api key
  
  # batch_size not technically necessary, but may help with extraction given size of dataset
  # can add a "field" argument to request more specific data, default is "all"
  
  #pb <- txtProgressBar(min = 0, max = 100, style = 3)  # Create progress bar
  
  show_loading_animation()
  
  
  # establish redcap connection
  df <- REDCapR::redcap_read(redcap_uri = uri,
                             token = token, 
                             forms = c("nda_study_intake",instrument_name),
                             batch_size = 1000,
                             verbose = TRUE)$data
  
  
 # df <- filter(df, between(df$src_subject_id, 10000, 71110)) # between seems() to cause error
                                                              # might be less flexible character to numeric
                                                              # src_subject_id may download as character sometimes
  df <- filter(df, src_subject_id > 10000, src_subject_id < 79110)
  # include guard clauses for mesaures that require aditional filtering beyond form name
  if (instrument_name == "scid_scoresheet") {
   df %>% select(contains(c("src_subject_id", "redcap_event_name", "scid_", "scip_", "mdd_", "pdd_"))) #scid_p18a was misspelled in the dataframe, that is why there is a "scip" variable :) 
  }
  
  # Close the progress bar
  #close(pb)
  
  # remove withdrawn and ineligible participants
  df %>% filter(phenotype < 4) -> df

  # recode phenotype (only need to recode phenotypes as 4 (ineligible) and 5 (withdrawn) have been removed in previous line)
  df %>% dplyr::mutate(phenotype=ifelse(phenotype==1, "hc",
                                        ifelse(phenotype==2,"chr",
                                               ifelse(phenotype==3,"hsc",NA)))) -> df

  # create a visit variable based on redcap_event_name
  ## not over-writing with rename(), so that redcap_event_name can do a "soft retire"
  df %>% dplyr::mutate(visit=redcap_event_name) -> df
  # we can't get rid of redcap_event_name because it is necessary for the event argument in getScidFiles.R
  # df <- subset(df, select = -redcap_event_name)
  
  # align redcap_event_name-ing convention with more natural language
  df %>% dplyr::mutate(visit=ifelse(visit=="baseline_arm_1", "bl",
                                        ifelse(visit=="12m_arm_1","12m",
                                               ifelse(visit=="24m_arm_1","24m",NA)))) -> df
  
  # create a site variable based on src_institution_name
  ## not over-writing with rename(), so that redcap_event_name can do a "soft retire"
  df %>% dplyr::mutate(site=src_institution_id) -> df
  # get rid of deprecated variable names is good practice
  df <- subset(df, select = -src_institution_id)
  
  # convert dates
  as.Date(df$interview_date, "%m/%d/%Y")
  
  # add measure column
  df$measure <- instrument_name
  
  # return task dataframe
  return(df);

}

getForms <- function() {
  
  forms <- REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = TRUE, config_options = NULL)$data
  # createCsv(forms)
  View(forms)
  return(forms)
  
}

getDictionary <- function(instrument_name) {
  
  metadata <- REDCapR::redcap_metadata_read(redcap_uri = uri, token = token, verbose = TRUE, config_options = NULL)$data
  dictionary <- metadata[metadata$form_name == instrument_name,]
  # View(dictionary)
  return(dictionary)

  
}
