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
#' @param id Optional; String, accepts "numeric" or "character", converts src_subject_id to desired format.
#' @return Prints the time taken for the data request process.
#' @export
#' @examples
#' dataRequest("prl", csv=TRUE)
#' dataRequest("rgpts", "kamin", rdata=TRUE)
#' 
#' #' @author Joshua Kenney <joshua.kenney@yale.edu>
#' 
#' 


dataRequest <- function(..., csv = FALSE, rdata = FALSE, spss = FALSE, id = NULL) {
  base::source("api/testSuite.R")

  # Required Libraries Setup
  if (!require("tidyverse")) {install.packages("tidyverse")}; library(tidyverse)
  if (!require("dplyr")) {install.packages("dplyr")}; library(dplyr)
  
  # Prepare lists for REDCap, Qualtrics, and tasks
  redcap_list <- tools::file_path_sans_ext(list.files("./clean/redcap"))
  qualtrics_list <- tools::file_path_sans_ext(list.files("./clean/qualtrics"))
  task_list <- tools::file_path_sans_ext(list.files("./clean/task"))

  start_time <- Sys.time()
  
  # Source necessary R scripts from the 'api' directory
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  lapply(list.files("api/fn", pattern = "\\.R$", full.names = TRUE), base::source)
  
  # Validate Measures Function
  validateMeasures <- function(data_list) {
    invalid_list <- Filter(function(measure) measure %!in% c(redcap_list, qualtrics_list, task_list), data_list)
    if (length(invalid_list) > 0) {
      stop(paste(invalid_list, collapse = ", "), " do not have a cleaning script, please create one!\n")
    }
  }
  
  # Compile data list and validate measures
  data_list <- list(...)
  validateMeasures(data_list)
  
  # Process each measure using processMeasure function
  for(measure in data_list) {
    sourceCategory <- ifelse(measure %in% redcap_list, "redcap", ifelse(measure %in% qualtrics_list, "qualtrics", "task"))
    processMeasure(measure, sourceCategory, csv, rdata, spss)
  }
  
  # Clean up and record processing time
  performCleanup()
  print(Sys.time() - start_time)  # Print time taken for processing
}



# Process Individual Measure Function
processMeasure <- function(measure, source, csv, rdata, spss) {
  # perform check to see if the measure is individual or combined
  # if individual, then do
  file_path <- sprintf("./clean/%s/%s.R", source, measure)
  message("\nProcessing ", measure, " from ", source, "...")
  result <- tryCatch({
    base::source(file_path)
    # Assuming testSuite is a function for running unit tests
    base::source("api/testSuite.R") # leaving this just in case people rm=(list=ls()) inside their cleaning script
    testSuite(measure, source, file_path)
    df_name <- paste0(measure, "_clean")
    # Assuming createExtract is a function to create data extracts
    createExtract(get(df_name), df_name, csv, rdata, spss)
  }, error = function(e) {
    message("Error with ", measure, ": ", e$message)
    NULL  # Return NULL on error
  })
  # if combined, then do
  # dataParse
  return(result)
}

# Cleanup Function
performCleanup <- function() {
  # Placeholder for cleanup operations, like disconnecting from databases
  suppressWarnings(source("api/env/cleanup.R"))
}
