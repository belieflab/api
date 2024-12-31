#' Data Request
#'
#' This function processes requests for clean data sequentially for specified measures.
#' It makes a request to the appropriate API for the named measure or measures
#' and runs the associated data cleaning routines. It then runs a series of
#' unit tests to verify that the data quality standardata are met.
#'
#' @param ... Strings, specifying the measures to process, which can be a Mongo collection, REDCap instrument, or Qualtrics survey.
#' @param csv Optional; Boolean, if TRUE creates a .csv extract in ./tmp.
#' @param rdata Optional; Boolean, if TRUE creates an .rdata extract in ./tmp.
#' @param spss Optional; Boolean, if TRUE creates a .sav extract in ./tmp.
#' @param identifier Optional; String, accepts "numeric" or "character", converts src_subject_id to desired format.
#' @return Prints the time taken for the data request process.
#' @export
#' @examples
#' dataRequest("prl", csv=TRUE)
#' dataRequest("rgpts", "kamin", rdata=TRUE)
#' 
#' #' @author Joshua Kenney <joshua.kenney@yale.edu>
#' 
#' 


dataRequest <- function(..., csv = FALSE, rdata = FALSE, spss = FALSE) {
  
  # Set up cleanup for any MongoDB connections that might persist
  on.exit({
    # Find and cleanup any mongo connections in the global environment
    mongo_objects <- ls(envir = .GlobalEnv, pattern = "^Mongo|_mongo$|^mongo", all.names = TRUE)
    for (obj in mongo_objects) {
      if (exists(obj, envir = .GlobalEnv)) {
        conn <- get(obj, envir = .GlobalEnv)
        if (is.environment(conn) && exists("disconnect", envir = conn)) {
          tryCatch({
            conn$disconnect()
          }, error = function(e) NULL)
        }
      }
    }
    gc()  # Force garbage collection
  })
  
  base::source("api/testSuite.R")
  
  # Required Libraries Setup
  if (!require("tidyverse")) {install.packages("tidyverse")}; library(tidyverse)
  if (!require("dplyr")) {install.packages("dplyr")}; library(dplyr)
  if (!require("config")) {install.packages("config")}; library(config)
  
  # Prepare lists for REDCap, Qualtrics, and tasks
  redcap_list <- tools::file_path_sans_ext(list.files("./clean/redcap"))
  qualtrics_list <- tools::file_path_sans_ext(list.files("./clean/qualtrics"))
  task_list <- tools::file_path_sans_ext(list.files("./clean/task"))
  
  # Get super_keys from config
  config <- config::get()
  super_keys <- config$super_keys
  if (is.null(super_keys) || super_keys == "") {
    stop("No super_keys specified in the config file.")
  }
  
  # Split super_keys if it's a comma-separated string
  if (is.character(super_keys)) {
    super_keys <- strsplit(super_keys, ",")[[1]]
  }
  
  start_time <- Sys.time()
  
  # Source necessary R scripts from the 'api' directory
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  lapply(list.files("api/fn", pattern = "\\.R$", full.names = TRUE), base::source)
  
  # Validate Measures Function
  validateMeasures <- function(data_list) {
    # Check if input is a dataframe
    if (is.data.frame(data_list)) {
      # Get the name of the dataframe as a string
      data_list <- deparse(substitute(data_list))
    }
    
    # Ensure data_list is a character vector (in case it's a single string)
    if (!is.character(data_list)) {
      data_list <- as.character(data_list)
    }
    
    # Validate measures against predefined lists
    invalid_list <- Filter(function(measure) !measure %in% c(redcap_list, qualtrics_list, task_list), data_list)
    
    if (length(invalid_list) > 0) {
      stop(paste(invalid_list, collapse = ", "), " does not have a cleaning script, please create one.\n")
    }
  }
  
  # Compile data list and validate measures
  data_list <- list(...)
  
  #this is so the function doesn't break if user enters a variable storing a character vector 
  #or a list of strings 
  #in other words it let's you do this:
  #vars_i_want <- c('demo','sps','sips_p')
  #dataRequest(vars_i_want)
  if (length(data_list) == 1){
    data_list = data_list[[1]]
  }
  validateMeasures(data_list)
  
  # Process each measure using processMeasure function
  for (measure in data_list) {
    sourceCategory <- ifelse(measure %in% redcap_list, "redcap", ifelse(measure %in% qualtrics_list, "qualtrics", "task"))
    processMeasure(measure, sourceCategory, csv, rdata, spss, super_keys)
  }
  
  # Clean up and record processing time
  # performCleanup()
  print(Sys.time() - start_time)  # Print time taken for processing
}

processMeasure <- function(measure, source, csv, rdata, spss, super_keys) {
  # Check if input is a dataframe
  if (is.data.frame(measure)) {
    # Get the name of the dataframe as a string
    measure <- deparse(substitute(measure))
  }
  
  # Ensure data_list is a character vector (in case it's a single string)
  if (!is.character(measure)) {
    measure <- as.character(measure)
  }
  # Construct the path to the measure's cleaning script
  file_path <- sprintf("./clean/%s/%s.R", source, measure)
  message("\nProcessing ", measure, " from clean/", source, "/", measure)
  
  # Setup cleanup on exit
  on.exit({
    if (exists("mongo_conn") && !is.null(mongo_conn)) {
      tryCatch({
        mongo_conn$disconnect()
      }, error = function(e) {
        warning(sprintf("Error disconnecting from MongoDB: %s", e$message))
      })
    }
    # Clear the mongo connection from memory
    if (exists("mongo_conn")) {
      rm(mongo_conn)
    }
    gc()  # Force garbage collection
  })
  
  result <- tryCatch({
    base::source(file_path)  # Execute the cleaning script for the measure
    # Ensure testSuite is sourced and then called
    base::source("api/testSuite.R")
    # Call testSuite with super_keys
    testSuite(measure, source, file_path, super_keys)
    
    df_name <- paste0(measure, "_clean")  # Construct the name of the cleaned data frame
    
    # Assuming createExtract is a function to create data extracts
    createExtract(get(df_name), df_name, csv, rdata, spss)  # Create data extracts
  }, error = function(e) {
    # Check if super_keys is valid (you can modify this logic based on your criteria)
    if (length(super_keys) == 0 || all(is.na(super_keys))) {
      message("An error occurred: ", e$message)  # General error message
    } else {
      message("Error with ", measure, ": ", e$message)  # Specific error message
    }
    NULL  # Return NULL on error
  })
  
  return(result)  # Return the result of the processing
}




# Add helper function for MongoDB cleanup
disconnectMongo <- function(mongo) {
  if (!is.null(mongo)) {
    tryCatch({
      mongo$disconnect()
      rm(list = deparse(substitute(mongo)), envir = parent.frame())
    }, error = function(e) {
      warning(sprintf("Error disconnecting from MongoDB: %s", e$message))
    })
  }
}


# Cleanup Function
performCleanup <- function() {
  # Placeholder for cleanup operations, like disconnecting from databases
  suppressWarnings(source("api/env/cleanup.R"))
}


### trying to store results from unit tests - work in progress (mkp)

# dataRequest <- function(..., csv = FALSE, rdata = FALSE, spss = FALSE, id = NULL) {
#   base::source("api/testSuite.R")
#   
#   # Load necessary libraries
#   if (!require("tidyverse")) {install.packages("tidyverse")}; library(tidyverse)
#   if (!require("dplyr")) {install.packages("dplyr")}; library(dplyr)
#   
#   # Prepare lists for REDCap, Qualtrics, and tasks
#   redcap_list <- tools::file_path_sans_ext(list.files("./clean/redcap"))
#   qualtrics_list <- tools::file_path_sans_ext(list.files("./clean/qualtrics"))
#   task_list <- tools::file_path_sans_ext(list.files("./clean/task"))
#   
#   start_time <- Sys.time()
#   
#   # Source necessary R scripts from the 'api' directory
#   lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
#   lapply(list.files("api/fn", pattern = "\\.R$", full.names = TRUE), base::source)
#   
#   # Compile data list and validate measures
#   data_list <- list(...)
#   validateMeasures <- function(data_list) {
#     invalid_list <- Filter(function(measure) !measure %in% c(redcap_list, qualtrics_list, task_list), data_list)
#     if (length(invalid_list) > 0) {
#       stop(paste(invalid_list, collapse = ", "), " do not have a cleaning script, please create one!\n")
#     }
#   }
#   validateMeasures(data_list)
#   
#   # Initialize variable to store aggregated results
#   all_test_results <- NULL
#   
#   # Process each measure using processMeasure function and collect results
#   for (measure in data_list) {
#     sourceCategory <- ifelse(measure %in% redcap_list, "redcap", ifelse(measure %in% qualtrics_list, "qualtrics", "task"))
#     test_results <- processMeasure(measure, sourceCategory, csv, rdata, spss)
#     all_test_results <- rbind(all_test_results, test_results)
#   }
#   
#   # Always create a CSV with all results
#   if (!is.null(all_test_results)) {
#     source("api/src/createCsv.R")
#     createCsv(all_test_results, paste0("combined_test_results_", format(Sys.Date(), "%Y%m%d")))  # Use custom function to create a single CSV
#     View(all_test_results)
#   }
#   
#   performCleanup()
#   print(Sys.time() - start_time)
# }
# 
# # Helper function for processMeasure is assumed to handle the unit tests and aggregate results as needed
# 
# 
# 
# 
# # Process Individual Measure Function
# processMeasure <- function(measure, source, csv, rdata, spss) {
#   file_path <- sprintf("./clean/%s/%s.R", source, measure)
#   message("\nProcessing ", measure, " from ", source, "...")
#   result <- tryCatch({
#     base::source(file_path)
#     base::source("api/testSuite.R")
#     test_results <- testSuite(measure, source, file_path)  # Gather test results
#     if (is.null(test_results)) {
#       test_results <- data.frame(Measure = measure, Test = "Test Suite Execution", Status = "Fail", Message = "No result returned from test suite")
#     }
#     test_results
#   }, error = function(e) {
#     message("Error with ", measure, ": ", e$message)
#     data.frame(Measure = measure, Test = "Error during processing", Status = "Fail", Message = e$message)
#   })
#   return(result)
# }
# 
# 
# # Cleanup Function
# performCleanup <- function() {
#   # Placeholder for cleanup operations, like disconnecting from databases
#   suppressWarnings(source("api/env/cleanup.R"))
# }
