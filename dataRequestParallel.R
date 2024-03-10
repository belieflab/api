#' Data Request
#'
#' The code below is meant to simplify requests for clean data.
#' It makes a request to the appropriate API for the named measure or measures
#' and runs the associated data cleaning routines. It then runs a series of
#' unit tests to verify the data quality standards are met to merge the data.
#' 
#' @param ... String, a Mongo collection, REDCap instrument, or Qualtrics survey
#' @param csv Optional; Boolean, create a .csv extract in ./tmp
#' @param rds Optional; Boolean, create a .Rds extract in ./tmp
#' @param spss Optional; Boolean create a .sav extract in ./tmp
#' @param id Optional; String accepts "numeric" or "character", converts src_subject_id
#'        to desired format
#' 
#' @example
#' 
#' dataRequest("prl")
#' dataRequest("rgpts,"kamin")
#' dataRequest("eds", csv=TRUE)
#' 
#' @author Joshua Kenney <joshua.kenney@yale.edu>




# The code below is meant to simplify requests for data
# It will aims to merge data on all requested measures and check for duplicates
# and other data quality issues.

# Not all measures have been cleaned and scored. These measures are clean and scored,
# therefore should be easy to obtain data for:


## redcap (interviewer-rated data): sips_p.R, sips_n.R, sips_d.R, sips_g.R, scid.R,
##                                  medlog.R, gfs.R, gfr.R, gaf_spd_criteria.R
## qualtrics (self-report data): lshsr.R, rgpts.R... [incomplete]
##
## behavioral task data: prl.R... [incomplete]


# Step 1: Connect to both the Yale and Northwestern University VPNs

# Step 2... need to (a) source all the scoring scripts and then merge all "_clean" datasets
# perhaps we can write a function for this?
# this is what I am imagining: merge(redcap=c("sips_p.R","scid.R"),
#                                    qualtrics=c("lshsr.R","rgpts.R"),
#                                    tasks=c("kamin.R")),
#                                    timepoints=c("baseline","12mo","24mo",
#                                    date="07/06/2023")
# function will create a merged dataset, print a note on any duplicates (based on id AND timepoint),
# remove any phenotypes > 4 and src_subject_id not in appropriate range
# something with date will allow us generic option of all data before last NDA upload
# OR will allow us to specify (e.g., most recent data)

# https://erdavenport.github.io/R-ecology-lesson/03-loops-and-functions.html

# Get full file paths of all R files in the api directory
# base::source all files using lapply()

# 12.18.2023, Trevor & Minerva: create csv argument that will produce a csv for _clean
### data frames, so that createCsv() can be removed from individual scripts

# Required Libraries Setup
if (!require("tidyverse")) {install.packages("tidyverse")}; library(tidyverse)
if (!require("dplyr")) {install.packages("dplyr")}; library(dplyr)
if (!require("future")) {install.packages("future")}; library(future)
if (!require("future.apply")) {install.packages("future.apply")}; library(future.apply)
if (!require("parallel")) {install.packages("parallel")}; library(parallel)

# Data processing and request management
dataRequestParallel <- function(..., csv = FALSE, rds = FALSE, spss = FALSE, id = NULL) {
  start_time <- Sys.time()
  
  # Source necessary R scripts from the 'api' directory
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  lapply(list.files("api/fn", pattern = "\\.R$", full.names = TRUE), base::source)
  base::source("api/testSuite.R")
  
  # Compile data list and validate measures
  data_list <- list(...)
  validateMeasures(data_list)
  
  # Now process 'task' measures separately (not in parallel)
  task_measures <- intersect(data_list, task_list)
  for(measure in task_measures) {
    processMeasure(measure, "task", csv, rds, spss)
  }
  
  # Setup parallel processing and process measures in parallel
  setupParallelEnvironment()
  processMeasuresInParallel(data_list, csv, rds, spss)
  
  # Clean up and record processing time
  performCleanup()
  print(Sys.time() - start_time)
}

validateMeasures <- function(data_list) {
  invalid_list <- Filter(function(measure) measure %!in% c(redcap_list, qualtrics_list, task_list), data_list)
  if (length(invalid_list) > 0) {
    stop(paste(invalid_list, collapse = ", "), " do not have a cleaning script, please create one!\n")
  }
}

setupParallelEnvironment <- function() {
  num_cores <- detectCores(logical = TRUE)
  plan(multisession, workers = max(1, num_cores - 2))  # Reserve 2 cores for system stability
}

processMeasuresInParallel <- function(data_list, csv, rds, spss) {
  # Exclude 'task' measures from parallel processing
  non_task_measures <- setdiff(data_list, task_list)
  sources <- list("redcap" = redcap_list, "qualtrics" = qualtrics_list)
  
  results <- lapply(names(sources), function(source) {
    measures <- intersect(non_task_measures, sources[[source]])
    future_lapply(measures, function(measure) {
      processMeasure(measure, source, csv, rds, spss)
    }, future.seed = TRUE)
  })
}

processMeasure <- function(measure, source, csv, rds, spss) {
  file_path <- sprintf("./clean/%s/%s.R", source, measure)
  message("\nProcessing ", measure, " from ", source, "...\n")
  result <- tryCatch({
    source(file_path)
    testSuite(measure, "redcap", file_path)
    df_name <- paste0(measure, "_clean")
    createExtract(get(df_name), df_name, csv, rds, spss)
  }, error = function(e) {
    message("Error with ", measure, ": ", e$message)
    NULL  # Return NULL on error
  })
  return(result)
}

performCleanup <- function() {
  # Placeholder for cleanup operations, like disconnecting from databases
  suppressWarnings(source("api/env/cleanup.R"))
}

redcap_list <- tools::file_path_sans_ext(list.files("./clean/redcap"))
qualtrics_list <- tools::file_path_sans_ext(list.files("./clean/qualtrics"))
task_list <- tools::file_path_sans_ext(list.files("./clean/task"))

