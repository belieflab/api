responsesToRemove <- function(df) {
  responses_to_remove <- df %>%
    # Convert 'interview_date' to Date type
    dplyr::mutate(interview_date = as.Date(interview_date, format = "%m/%d/%Y")) %>%
    
    # Add a grouping count by 'src_subject_id' and 'visit' to identify duplicates based on both fields
    dplyr::add_count(src_subject_id, visit) %>%
    
    # Keep rows where there are duplicates based on both 'src_subject_id' and 'visit'
    dplyr::filter(n > 1) %>%
    
    # Remove the temporary count column for clarity
    dplyr::select(-n) %>%
    
    # Group by 'src_subject_id' and 'visit' for processing
    dplyr::group_by(src_subject_id, visit) %>%
    
    # Determine the latest date in each group
    dplyr::mutate(max_date_per_group = base::max(interview_date)) %>%
    
    # Filter out the row with the latest date only if it's a duplicate in both 'src_subject_id' and 'visit'
    dplyr::filter(interview_date != max_date_per_group) %>%
    
    # Clean up the data by removing the temporary columns
    dplyr::select(-max_date_per_group) %>%
    dplyr::ungroup()  # Ensure no grouping is left over
  
  source("api/src/createCsv.R")
  createCsv("responses_to_remove")
  view(responses_to_remove)
}