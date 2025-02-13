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

start_time <- Sys.time()

ndaRequest <- function(..., csv = FALSE, rdata = FALSE, spss = FALSE) {
  
  base::source("api/getRedcap.R")
  base::source("api/getSurvey.R")
  base::source("api/getTask.R")
  
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
  
  base::source("api/ndaValidator.R")
  
  # Required Libraries Setup
  if (!require("tidyverse")) {install.packages("tidyverse")}; library(tidyverse)
  if (!require("dplyr")) {install.packages("dplyr")}; library(dplyr)
  if (!require("config")) {install.packages("config")}; library(config)
  if (!require("beepr")) {install.packages("beepr")}; library(beepr)
  
  # Prepare lists for REDCap, Qualtrics, and tasks
  redcap_list <- tools::file_path_sans_ext(list.files("./nda/redcap"))
  qualtrics_list <- tools::file_path_sans_ext(list.files("./nda/qualtrics"))
  task_list <- tools::file_path_sans_ext(list.files("./nda/mongo"))
  
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
      stop(paste(invalid_list, collapse = ", "), " does not have a cleaning script, please create one in nda/.\n")
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
    api <- ifelse(measure %in% redcap_list, "redcap", ifelse(measure %in% qualtrics_list, "qualtrics", "mongo"))
    processMeasure(measure, api, csv, rdata, spss, super_keys, start_time)
  }
  
  # Clean up and record processing time
  # performCleanup()
  # print(Sys.time() - start_time)  # Print time taken for processing
}

processMeasure <- function(measure, api, csv, rdata, spss, super_keys, start_time) {
  # Check if input is a dataframe
  if (is.data.frame(measure)) {
    # Get the name of the dataframe as a string
    measure_name <- deparse(substitute(measure))
  } else {
    measure_name <- measure
  }
  
  # Ensure data_list is a character vector (in case it's a single string)
  if (!is.character(measure)) {
    measure <- as.character(measure)
  }
  
  # Construct the path to the measure's cleaning script
  file_path <- sprintf("./nda/%s/%s.R", api, measure)
  message("\nFetching ", measure, " with nda/", api, "/", measure,".R\n")
  
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
    # Apply date format preservation after processing
    # Get the data frame from global environment
    df <- get0(measure, envir = .GlobalEnv)
    
    # Only process if df exists and is a data frame
    if (!is.null(df) && is.data.frame(df)) {
      df <- preserveDateFormat(df)
      # Reassign the processed data frame
      assign(measure, df, envir = .GlobalEnv)
    }
    
    # Run validation
    base::source("api/ndaValidator.R")
    validation_results <- ndaValidator(measure, api)
    # Create data upload template if test passes
    if (validation_results$valid == TRUE) {
      beepr::beep("mario")
    }
    
    if (validation_results$valid == FALSE) {
      beepr::beep("wilhelm")
    }
    
    base::source("api/src/ndaTemplate.R")
    ndaTemplate(measure)
    formatElapsedTime(start_time)
    
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

# Helper function to preserve MM/DD/YYYY format
preserveDateFormat <- function(df) {
  if ("interview_date" %in% names(df)) {
    df <- df %>%
      dplyr::mutate(interview_date = format(as.Date(interview_date), "%m/%d/%Y"))
  }
  return(df)
}

# Helper function to display time savings.
formatElapsedTime <- function(start_time) {
  time_diff <- Sys.time() - start_time
  units <- attr(time_diff, "units")
  
  formatted_time <- switch(units,
                           "secs" = sprintf("%.1f seconds", time_diff),
                           "mins" = sprintf("%.1f minutes", time_diff),
                           sprintf("%.1f %s", time_diff, units)
  )
  
  message("Formatted for NDA in ", formatted_time, ".")
}
