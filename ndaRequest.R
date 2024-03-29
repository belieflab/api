# sister script of dataRequest for NDA uploads


ndaRequest <- function(...) {
  
  # dependencies
  source("api/getRedcap.R")
  source("api/getSurvey.R")
  source("api/getTask.R")
  source("api/ndaSuite.R")
  
  # DATA RETRIEVAL
  # get list of all files in api/src that end in .R (get list of all R scripts);
  # source al of those files
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)

  if (!require(tidyverse)) { install.packages("tidyverse") }; library(tidyverse)
  if (!require(dplyr)) { install.packages("dplyr") }; library(dplyr)
  
  # init an empty list called data_list
  data_list <- list(...)
  
  # List all files in the cleaning directories
  redcap_list <- list.files("clean/redcap/")
  qualtrics_list <- list.files("clean/qualtrics/")
  task_list <- list.files("clean/task/")

  # Remove file extensions
  redcap_list <- tools::file_path_sans_ext(redcap_list)
  qualtrics_list <- tools::file_path_sans_ext(qualtrics_list)
  task_list <- tools::file_path_sans_ext(task_list)

  # init empty list called invalid_list to store invalid measures
  invalid_list <- list()

  # check if measures in data_list are not found in the list of valid measures;
  # if they are not found in the lists of valid measures (from redcap, qualtrics, and task),
  # add those to invalid_list
  for (i in 1:length(data_list)) {
    if (data_list[i] %!in% redcap_list && data_list[i] %!in% qualtrics_list && data_list[i] %!in% task_list) {
      invalid_list <- c(invalid_list, data_list[i])
    }
  }

  if (length(invalid_list) > 0) {
    # output on how to remedy
    message("Check that each measure is spelled correctly and exists in these lists:\n")

    message("REDCap: ")
    cat(paste(redcap_list), "\n")

    message("Qualtrics: ")
    cat(paste(qualtrics_list), "\n")

    message("Tasks: ")
    cat(paste(task_list), "\n\n")


    # abort routine if any measure names are invalid
    for (i in 1:length(invalid_list)) {
      stop(invalid_list[i], " is not a valid measure name!\n")
    }
  }

  # source redcap cleaning scripts to obtain data frames
  for (i in 1:length(data_list)) {
    if (data_list[i] %in% redcap_list) {
      # use getter
      df <- getRedcap(data_list[[i]])
      cat("\n")
      cat(paste("fetching", data_list[i], "..."))
      cat("\n")
      # unit testing
      ndaSuite(data_list[i], "redcap", df)
      # get NDA prefix
      script_path <- paste0("clean/redcap/", data_list[i], ".R")
      nda_prefix <- getNdaPrefix(script_path)
      # create NDA extract
      ndaTemplate(df,nda_prefix)
    }
  }
  
  # source qualtrics cleaning scripts to obtain data frames
  for (i in 1:length(data_list)) {
    if (data_list[i] %in% qualtrics_list) {
      # use getter
     #assign(data_list[[i]], getSurvey(data_list[[i]])) # Create variable with the name from data_list[[i]]
      df <- getSurvey(data_list[[i]])
      cat("\n")
      cat(paste("fetching", data_list[i], "..."))
      cat("\n")
      
      # List of columns to remove
      cols_to_remove <- c("StartDate", "EndDate", "Status", "Progress", "Duration (in seconds)", 
                          "Finished", "RecordedDate", "ResponseId", "DistributionChannel", 
                          "UserLanguage", "candidateId", "studyId", "measure", "ATTN", "ATTN_1", "SC0")
      
      # Remove columns only if they exist in df
      df <- df %>% select(-any_of(cols_to_remove))
      
      # unit testing
      ndaSuite(data_list[i], "qualtrics", df)
      # get NDA prefix
      # script_path <- paste0("clean/qualtrics/", data_list[i], ".R")
      # nda_prefix <- getNdaPrefix(script_path)
      # Using switch to modify nda_prefix
      nda_prefix <- switch(data_list[[i]],
                           "cesd" = "ces_d,1",
                           "demo" = "demographics,2",
                           "duf" = "duf,1",
                           "lec" = "lec,1",
                           "pqb" = "pq,1",
                           "pss" = "pss,1",
                           data_list[[i]]  # default case to leave the value unchanged
      )
      # create NDA extract
      ndaTemplate(df,nda_prefix)
    }
  }
  
  # source task cleaning scripts to obtain data frames
  for (i in 1:length(data_list)) {
    if (data_list[i] %in% task_list) {
      # use getter
      df <- getTask(data_list[[i]])
      cat("\n")
      cat(paste("fetching", data_list[i], "..."))
      cat("\n")
      
      # List of columns to remove
      cols_to_remove <- c("stimulus","key_press","trial_index","trial_type","time_elapsed","internal_node_id","test_part","visit","measure")
      
      # Remove columns only if they exist in df
      df <- df %>% select(-any_of(cols_to_remove))
      
      if (data_list[[i]] == "eefrt") {
        
        cols_to_include <- c("eefrt_01_condition", "eefrt_01_taps", "hard_reward_magnitude", 
                          "interview_age", "interview_date", "reward_hard", "index",
                          "sex", "site", "src_subject_id", "subjectkey")
        
        # Subset the dataframe to include only the specified columns
        df <- select(df, all_of(cols_to_include))
        
        # Rename columns
        # names(df)[names(df) == "old_name1"] <- "new_name1"
        names(df)[names(df) == "hard_reward_magnitude"] <- "reward_sensitivity_beta"
        names(df)[names(df) == "reward_hard"] <- "total_hard_choice"
        
      }
      
      # rename index to trial
      names(df)[names(df) == "index"] <- "trial"
      
      
    
      
      # unit testing
      ndaSuite(data_list[i], "task", df)
      # get NDA prefix
      # script_path <- paste0("clean/task/", data_list[i], ".R")
      # nda_prefix <- getNdaPrefix(script_path)
      # Using switch to modify nda_prefix
      nda_prefix <- switch(data_list[[i]],
                           "eefrt" = "eefrt,1",
                           "dd" = "deldisk,1",
                           "ch" = "conhal,1",
                           data_list[[i]]  # default case to leave the value unchanged
      )
      # create NDA extract
      ndaTemplate(df,nda_prefix)
    }
  }

  
}
