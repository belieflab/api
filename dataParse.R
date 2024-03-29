# Input: string representation of the SINGLE raw qualtrics dataframe
# Output: MULTIPLE subsets of the dataframes for each questionnaire


dataParse <- function(qualtrics_alias){
  
  # Step 0: Ensure short name of study matches qualtrics survey name
  
  # Step 1: Get survey from qualtrics
  df <- getSurvey(qualtrics_alias)
  
  # Step 2: Determine candidate keys
  candidate_keys <- c("participantId",
                      "workerId",
                      "PROLIFIC_ID",
                      "interview_date",
                      "completion_code")
  
  # Step 3: Compare the available candidate keys in the dataframe to candidate_keys vector
  # and select the relevant ones as candidate_keys_subset
  candidate_keys_subset <- NULL
  
  # Step 4: Determine the prefixes used e.g., rgpts_, lshr_, bai_, and store
  # to variable survey_prefixes
  survey_prefixes <- NULL
  
  # Step 5: Subset dataframe based on survey_prefixes and include candidate_keys_subset
  # for each.
  
}

