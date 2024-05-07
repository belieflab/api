# Input: string representation of the SINGLE raw qualtrics dataframe
# Output: MULTIPLE subsets of the dataframes for each questionnaire


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



