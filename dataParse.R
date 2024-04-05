# Input: string representation of the SINGLE raw qualtrics dataframe
# Output: MULTIPLE subsets of the dataframes for each questionnaire


dataParse <- function(qualtrics_alias){
  
  # Step 0: Ensure short name of study matches qualtrics survey name
  source("api/getSurvey.R") # call getSurvey()
  
  # Step 1: Get survey from qualtrics
  df <- getSurvey(qualtrics_alias)
  
  # Step 2: Determine candidate keys
  candidate_keys <- c("participantId",
                      "workerId",
                      "PROLIFIC_PID",
                      "interview_date",
                      "CompletionCode")
  
  # Step 3: Compare the available candidate keys in the dataframe to candidate_keys vector
  # and select the relevant ones as candidate_keys_subset
  candidate_keys_subset <- NULL
  
  # Step 4: Determine the prefixes used e.g., rgpts_, lshr_, bai_, and store
  # to variable survey_prefixes
  survey_prefixes <- c('demo_', # demographics 
                       'rgpts_', # revised-green et al paranoid thought scale
                       'bpe_', # belief in the purpose of events
                       'as_', # absorption scale
                       'ses_', # spiritual events scale
                       'ps_', # porosity scale
                       'pv_' # porosity vignette
                       ) 
  
  # Step 5: Subset dataframe based on survey_prefixes and include candidate_keys_subset
  # for each.
  
 list(demographics = df[, c(candidate_keys, grep(survey_prefixes[1], names(df), value = TRUE))],
       paranoia = df[, c(candidate_keys, grep(survey_prefixes[2], names(df), value = TRUE))],
       teleology = df[, c(candidate_keys, grep(survey_prefixes[3], names(df), value = TRUE))],
       absorption = df[, c(candidate_keys, grep(survey_prefixes[4], names(df), value = TRUE))],
       spiritual_events = df[, c(candidate_keys, grep(survey_prefixes[5], names(df), value = TRUE))],
       porosity = df[, c(candidate_keys, grep(survey_prefixes[6], names(df), value = TRUE))],
       porosity_vignette = df[, c(candidate_keys, grep(survey_prefixes[7], names(df), value = TRUE))])
  
 

}

dat <- dataParse("prl_pilot")

demographics <- dat$demographics
rgpts <- dat$paranoia



