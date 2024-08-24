# Input: string representation of the SINGLE raw qualtrics dataframe
# Output: MULTIPLE subsets of the dataframes for each questionnaire

# process a full dataframe that includes a collection of surveys to separate
#   into individual dataframes for each survey including only the correct
#   identifier: participantId, workerId, PROLIFIC_PID, or src_subject_id (default in getSurvey())




dataParse <- function(qualtrics_alias, label){
  if (!require("dplyr")) {install.packages("dplyr"); library(dplyr)}
  
  source("api/getSurvey.R")
  df <- getSurvey(qualtrics_alias, label = label)
  
  # Define potential identifiers
  super_keys <- c("participantId", "workerId", "PROLIFIC_PID", "src_subject_id")
  
  # Filter to keep only existing keys in the dataframe
  existing_keys <- super_keys[super_keys %in% names(df)]

  # Check if any identifiers exist in the dataframe
  if (length(existing_keys) == 0) {
    stop("No valid identifiers found in the dataframe.")
  }
  
  # Print existing identifiers for debugging
  print("Existing identifiers:")
  print(existing_keys)
  
  # Find the first identifier with non-NA values
  identifier <- NA
  for (key in existing_keys) {
    non_na_count <- sum(!is.na(df[[key]]))
    
    # Debug print to check how many non-NA values exist in each column
    print(paste("Checking identifier:", key, "with", non_na_count, "non-NA values"))
    
    if (non_na_count > 0) {  # As long as there's at least 1 non-NA value
      identifier <- key
      break
    }
  }
  
  # If no column has any non-NA values, issue a warning or stop
  if (is.na(identifier)) {
    stop(paste("No identifier found without NA values or multiple identifiers exist:", existing_keys))
  }
  
  # Print the detected identifier for debugging
  print(paste("Detected identifier:", identifier))
  
  # Exclude non-survey and specific columns
  non_survey_columns <- c(existing_keys, "interview_date", "PROLIFIC_PID")
  survey_columns <- names(df)[!names(df) %in% non_survey_columns & grepl("_", names(df))]
  
  # Extract unique survey prefixes from survey-specific column names
  extract_first_part <- function(string) {
    parts <- strsplit(string, "_")[[1]]
    return(parts[1])
  }
  survey_prefixes <- unique(sapply(survey_columns, extract_first_part))
  
  # Exclude prefixes that might still be problematic
  excluded_prefixes <- c("PROLIFIC", "interview")
  survey_prefixes <- survey_prefixes[!survey_prefixes %in% excluded_prefixes]
  
  # Create a list of dataframes, one for each survey
  output = list()
  for (prefix in survey_prefixes) {
    survey_specific_columns <- grep(paste0("^", prefix, "_"), names(df), value = TRUE)
    if (length(survey_specific_columns) > 0) {
      subset_df <- df[, c(identifier, survey_specific_columns)]
      output[[prefix]] <- subset_df
    }
  }
  
  names(output) <- survey_prefixes
  
  return(list2env(output, globalenv()))
}


