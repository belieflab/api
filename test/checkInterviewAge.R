################## Test 5 ##################


checkInterviewAge <- function(measure_alias) {
  
  if(!require(tidyverse)) {install.packages("tidyverse")};library(tidyverse)
  
  months_in_a_year <- 12
  
  min_age <- 12 * months_in_a_year
  max_age  <- 70 * months_in_a_year
  
  # append _clean to the measure in question
  output_df_name <- paste0(measure_alias, "_clean")
  
  # store clean dataframe in df_clean
  df_clean <- base::get(output_df_name)
  
  test_that(paste0("Check interview_age is between ",min_age," and ",max_age,"."), {
    
    rows_not_meeting_condition <- df_clean$src_subject_id[df_clean$interview_age <= min_age | df_clean$interview_age >= max_age]
    
    tryCatch({
      
      test_that::expect_true(
        all(df_clean$interview_age >= min_age & df_clean$interview_age <= max_age),
        info = paste("All values in 'interview_age' should be greater than than", min_age, "and less than", max_age,". src_subject_id not meeting condition:", paste(rows_not_meeting_condition, collapse = ", "))
        
      )}, error = function(e) {
        message("FindTextInScript failed: ", e$message)
        
      }
      
    )
  })
}

