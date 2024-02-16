################## Test 5 ##################


checkInterviewAge <- function(measure_alias) {
  
  if (!require(testthat)) {install.packages("testthat")}; library(testthat)
  
  months_in_a_year <- 12
  min_age <- 12 * months_in_a_year  # 144 months
  max_age <- 70 * months_in_a_year  # 840 months
  
  # Construct the expected dataframe name
  output_df_name <- paste0(measure_alias, "_clean")
  
  # Retrieve the dataframe based on constructed name
  df_clean <- base::get(output_df_name)  # specify the environment if needed
  tryCatch({
    # Perform tests
    test_that(paste0("Check interview_age is between ", min_age, " and ", max_age), {
      rows_not_meeting_condition <- df_clean$src_subject_id[df_clean$interview_age < min_age | df_clean$interview_age > max_age]
      
      testthat::expect_true(
        all(df_clean$interview_age >= min_age & df_clean$interview_age <= max_age),
        info = paste("All values in 'interview_age' should be greater than ", min_age, " and less than ", max_age, ". src_subject_id not meeting condition:", paste(rows_not_meeting_condition, collapse = ", "))
      )
    })
  }, error = function(e) {
    message("The following subjects have out of range ages:",rows_not_meeting_condition, e$message)
  })
}