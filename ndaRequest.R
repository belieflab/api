#' Process and Prepare Data for NDA Submission
#'
#' This function processes specified measures, validates them, and prepares the data
#' for submission to the National Data Archive (NDA). It handles both Qualtrics surveys
#' and task-based measures.
#'
#' @param ... Character strings specifying the measures to process. These can be 
#'   Qualtrics survey names or task names.
#'
#' @return This function does not return a value. It processes the specified measures
#'   and creates NDA templates as a side effect.
#'
#' @import tidyverse
#' @import dplyr
#' @importFrom tools file_path_sans_ext
#' @importFrom config get
#'
#' @examples
#' ndaRequest("cesd", "eefrt")
#' ndaRequest("pss", "dd", "ch")
#'
#' @export
ndaRequest <- function(...) {
  
  source("api/ndaSuite.R")
  
  # Ensure surveyIds and other necessary variables are loaded
  config <- config::get()
  source(config$qualtrics$survey_ids)
  
  # Load required libraries
  if (!require(tidyverse)) { install.packages("tidyverse") }; library(tidyverse)
  if (!require(dplyr)) { install.packages("dplyr") }; library(dplyr)
  
  # Source all R scripts in api/src
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  
  # Prepare lists for REDCap, Qualtrics, and tasks
  redcap_list <- tools::file_path_sans_ext(list.files("clean/redcap/"))
  qualtrics_list <- tools::file_path_sans_ext(list.files("clean/qualtrics/"))
  task_list <- tools::file_path_sans_ext(list.files("clean/task/"))
  
  # Compile data list
  data_list <- c(...)
  
  # Validate measures
  validateMeasures(data_list, redcap_list, qualtrics_list, task_list)
  
  # Process each measure
  for (measure in data_list) {
    if (measure %in% qualtrics_list) {
      processQualtricsMeasure(measure)
    } else if (measure %in% task_list) {
      processTaskMeasure(measure)
    }
  }
}

#' Validate Measure Names
#'
#' This function checks if the provided measure names are valid (i.e., exist in the 
#' REDCap, Qualtrics, or task lists).
#'
#' @param data_list A character vector of measure names to validate.
#' @param redcap_list A character vector of valid REDCap measure names.
#' @param qualtrics_list A character vector of valid Qualtrics measure names.
#' @param task_list A character vector of valid task measure names.
#'
#' @return This function does not return a value. It stops execution with an error 
#'   message if any invalid measure names are found.
#'
#' @keywords internal
validateMeasures <- function(data_list, redcap_list, qualtrics_list, task_list) {
  invalid_list <- data_list[!data_list %in% c(redcap_list, qualtrics_list, task_list)]
  if (length(invalid_list) > 0) {
    stop(paste(invalid_list, collapse = ", "), " are not valid measure names!")
  }
}


#' Process Qualtrics Measure
#'
#' This function processes a single Qualtrics measure. It fetches the data, removes 
#' specified columns, runs the ndaSuite, and creates an NDA template.
#'
#' @param measure A character string specifying the Qualtrics measure to process.
#'
#' @return This function does not return a value. It processes the measure and 
#'   creates an NDA template as a side effect.
#'
#' @keywords internal
processQualtricsMeasure <- function(measure) {
  base::source("api/getSurvey.R")
  cat("\nFetching", measure, "from Qualtrics...\n")
  df <- getQualtrics(measure)
  
  # Remove specified columns
  cols_to_remove <- c("StartDate", "EndDate", "Status", "Progress", "Duration (in seconds)", 
                      "Finished", "RecordedDate", "ResponseId", "DistributionChannel", 
                      "UserLanguage", "candidateId", "studyId", "measure", "ATTN", "ATTN_1", "SC0")
  df <- df[!names(df) %in% cols_to_remove]
  
  # Run ndaSuite
  ndaSuite(measure, "qualtrics", df)
  
  # Get NDA prefix using switch
  nda_prefix <- switch(measure,
                       "cesd" = "ces_d,1",
                       "demo" = "demographics,2",
                       "duf" = "duf,1",
                       "lec" = "lec,1",
                       "pqb" = "pq,1",
                       "pss" = "pss,1",
                       measure)  # default case
  
  # Create NDA template
  ndaTemplate(df, nda_prefix)
}

#' Process Task Measure
#'
#' This function processes a single task measure. It fetches the data, removes 
#' specified columns, performs task-specific processing if needed, runs the ndaSuite, 
#' and creates an NDA template.
#'
#' @param measure A character string specifying the task measure to process.
#'
#' @return This function does not return a value. It processes the measure and 
#'   creates an NDA template as a side effect.
#'
#' @keywords internal
processTaskMeasure <- function(measure) {
  base::source("api/getTask.R")
  cat("\nFetching", measure, "from tasks...\n")
  df <- getTask(measure)
  
  # Remove specified columns
  cols_to_remove <- c("stimulus", "key_press", "trial_index", "trial_type", "time_elapsed", 
                      "internal_node_id", "test_part", "visit", "measure")
  df <- df[!names(df) %in% cols_to_remove]
  
  # Task-specific processing (e.g., for eefrt)
  if (measure == "eefrt") {
    cols_to_include <- c("eefrt_01_condition", "eefrt_01_taps", "hard_reward_magnitude",
                         "interview_age", "interview_date", "reward_hard", "index",
                         "sex", "site", "src_subject_id", "subjectkey")
    df <- df[, cols_to_include]
    names(df)[names(df) == "hard_reward_magnitude"] <- "reward_sensitivity_beta"
    names(df)[names(df) == "reward_hard"] <- "total_hard_choice"
  }
  
  # Rename 'index' to 'trial'
  names(df)[names(df) == "index"] <- "trial"
  
  # Run ndaSuite
  ndaSuite(measure, "task", df)
  
  # Get NDA prefix using switch
  nda_prefix <- switch(measure,
                       "eefrt" = "eefrt,1",
                       "dd" = "deldisk,1",
                       "ch" = "conhal,1",
                       measure)  # default case
  
  # Create NDA template
  ndaTemplate(df, nda_prefix)
}