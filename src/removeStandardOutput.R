
removeStandardOutput <- function(df) {
  # List of Qualtrics columns to remove
  cols_to_remove <- c("StartDate", "EndDate", "Status", "Progress", "Duration (in seconds)", 
                      "Finished", "RecordedDate", "ResponseId", "DistributionChannel",
                      "UserLanguage", "candidateId", "studyId", "measure", "ATTN", "ATTN_1", "SC0")
  
  # Remove columns only if they exist in df
  df <- df %>% dplyr::select(-dplyr::any_of(cols_to_remove))
  return(df)
}
