# Input: string representation of the SINGLE raw qualtrics dataframe
# Output: MULTIPLE subsets of the dataframes for each questionnaire

# process a full dataframe that includes a collection of surveys to separate
#   into individual dataframes for each survey including only the correct
#   identifier: participantId, workerId, PROLIFIC_PID, or src_subject_id (default in getSurvey())

dataParse <- function(qualtrics_alias){
  if (!require("dplyr")) {install.packages("dplyr")}; library(dplyr)
  
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
  
  # Step 3: remove candidate keys interview_date and PROLIFIC_PID because
  # they have underscores and we only want survey prefixes
  prefixes <- dplyr::select(df, -interview_date, -PROLIFIC_PID)
  # get column names that include "_"
  prefixes <- colnames(prefixes)[grepl("_", colnames(prefixes))]
  
  # Step 4: Extract prefixes before first occurrence of "_"
  extract_first_part <- function(string) {
    parts <- strsplit(string, "_")[[1]]
    return(parts[1])
  }
  survey_prefixes <- unique(sapply(prefixes, extract_first_part))
  
  # Step 5: Subset dataframe based on survey_prefixes
  output = list()

  for (i in 1:length(survey_prefixes)) {
    
    if (sum(grep(survey_prefixes[i], colnames(df))) > 0) { # if name of column contains survey_prefix[1], then this survey exists in this database, so add to list
      
      # add to list
      output[[i]] <- df[, c(candidate_keys, grep(survey_prefixes[i], colnames(df), value = TRUE))] 
      
    } else {
      # which does not exist?
      output[[i]] <- survey_prefixes[i]
      
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


