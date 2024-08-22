# Input: string representation of the SINGLE raw qualtrics dataframe
# Output: MULTIPLE subsets of the dataframes for each questionnaire

# process a full dataframe that includes a collection of surveys to separate
#   into individual dataframes for each survey including only the correct
#   identifier: participantId, workerId, PROLIFIC_PID, or src_subject_id (default in getSurvey())

dataParse <- function(qualtrics_alias){
  if (!require("dplyr")) {install.packages("dplyr"); library(dplyr)}
  
  source("api/getSurvey.R")
  df <- getSurvey(qualtrics_alias)
  
  # Define potential identifiers
  super_keys <- c("participantId", "workerId", "PROLIFIC_PID", "src_subject_id")
  
  # Filter to keep only existing keys in the dataframe
  existing_keys <- super_keys[super_keys %in% names(df)]

  # Check if any identifiers exist in the dataframe
  if (length(existing_keys) == 0) {
    stop("No valid identifiers found in the dataframe.")
  }
  
  # Find the first identifier with no NA values
  identifier <- NA
  for (key in existing_keys) {
    if (all(!is.na(df[[key]]))) {
      identifier <- key
      break
    }
  }
  
  # If no column is completely free of NA values, issue a warning or stop
  if (is.na(identifier)) {
    stop(paste("No identifier found without NA values or multiple identifiers exist:", existing_keys))
  }
  
  # Print the detected identifier for debugging
  print(paste("Detected identifier:", identifier))
  
  # Exclude non-survey and specific columns
  non_survey_columns <- c(existing_keys, "interview_date", "PROLIFIC_PID")
  survey_columns <- names(df)[!names(df) %in% non_survey_columns & grepl("_", names(df))]
  # print(survey_columns)
  
  # Extract unique survey prefixes from survey-specific column names
  extract_first_part <- function(string) {
    parts <- strsplit(string, "_")[[1]]
    return(parts[1])
  }
  survey_prefixes <- unique(sapply(survey_columns, extract_first_part))
  
  # Exclude prefixes that might still be problematic
  excluded_prefixes <- c("PROLIFIC", "interview")
  survey_prefixes <- survey_prefixes[!survey_prefixes %in% excluded_prefixes]
  
  # Print detected survey prefixes for debugging
  # print("Detected survey prefixes:")
  # print(survey_prefixes)
  
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
  
  # return(output)
  return(list2env(output,globalenv()))
}



# dat <- dataParse("prl_pilot")

### mkp dynamic identifier selection
# getSurvey will get the correct identifier in the df
# dataParse <- function(qualtrics_alias){
#   if (!require("dplyr")) {install.packages("dplyr")}; library(dplyr)
#   
#   source("api/getSurvey.R")  # Load the getSurvey function
#   
#   # Get the survey data, assuming the identifier is properly included and handled in getSurvey
#   df <- getSurvey(qualtrics_alias)
#   
#   # Define necessary columns including 'interview_date' and 'CompletionCode'
#   required_keys <- c("interview_date", "CompletionCode")
#   
#   # Assuming getSurvey includes the identifier, detect it dynamically
#   super_keys <- c("participantId", "workerId", "PROLIFIC_PID", "src_subject_id")
#   # Find the first super key that is actually present in the dataframe
#   identifier <- names(df)[names(df) %in% super_keys][1]
#   
#   # Include these keys plus the dynamically determined identifier in the subset operations
#   candidate_keys <- c(identifier, required_keys)
#   
#   # Ensure these keys exist in the dataframe before proceeding
#   existing_keys <- intersect(candidate_keys, colnames(df))
#   
#   # Extract prefixes before the first occurrence of "_"
#   extract_first_part <- function(string) {
#     parts <- strsplit(string, "_")[[1]]
#     return(parts[1])
#   }
#   survey_prefixes <- unique(sapply(names(df)[grepl("_", names(df))], extract_first_part))
#   
#   # Creating subsets of the dataframe based on these prefixes
#   output <- list()
#   for (prefix in survey_prefixes) {
#     survey_columns <- grep(paste0("^", prefix, "_"), names(df), value = TRUE)
#     if (length(survey_columns) > 0) {
#       subset_df <- df %>%
#         select(all_of(existing_keys), all_of(survey_columns))
#       output[[prefix]] <- subset_df
#     }
#   }
#   
#   names(output) <- survey_prefixes  # Name the list elements based on prefixes
#   
#   return(output)
# }


