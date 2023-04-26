#
# function: getRedcap(instrumentName)
# input: instrumentName from table below
#

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

getRedcap <- function(instrumentName) {
  
  # documentation
  # https://www.richardshanna.com/tutorial/redcapapi/
  
  # installs REDCapR if not already installed; load REDCapR
  if(!require(REDCapR)) {install.packages("REDCapR")}; library(REDCapR);
  if(!require(tidyverse)) {install.packages("tidyverse")};library(tidyverse)
  
  # check to see if secrets.R exists; if it does not, create it
  if (!file.exists("secrets.R")) {
    file.create("secrets.R");
    return(print("secrets.R file created. please add uri, token"));
  }
  
  source("secrets.R"); # sensitive info for api key
  
  # batch_size not technically necessary, but may help with extraction given size of dataset
  # can add a "field" argument to request more specific data, default is "all"

  df <- REDCapR::redcap_read(redcap_uri = uri,
                             token = token, 
                             forms = c("nda_study_intake",instrumentName),
                             batch_size = 1000,
                             verbose = TRUE)$data
  df <- filter(df, between(df$src_subject_id, 10000, 71110))
  
  # include guard clauses for mesaures that require aditional filtering beyond form name
  if (instrumentName == "scid_scoresheet") {
   df %>% select(contains(c("src_subject_id", "redcap_event_name", "scid_", "scip_", "mdd_", "pdd_"))) #scid_p18a was misspelled in the dataframe, that is why there is a "scip" variable :) 
  }
  # return task dataframe
  return(df);

}

getForms <- function() {
  
  forms <- REDCapR::redcap_instruments(redcap_uri = uri, token = token, verbose = TRUE, config_options = NULL)$data
  createCsv(forms)
  return(View(forms))
  
}

getDictionary <- function(instrumentName) {
  
  metadata <- REDCapR::redcap_metadata_read(redcap_uri = uri, token = token, verbose = TRUE, config_options = NULL)$data
  dictionary <- metadata[metadata$form_name == instrumentName,]
  # View(dictionary)
  return(dictionary)

  
}
