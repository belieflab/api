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

source("testSuite.R")


dataRequest <- function(...) {
  # DATA RETRIEVAL
  # get list of all files in api/src that end in .R (get list of all R scripts);
  # source al of those files
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)

  if (!require(tidyverse)) {
    install.packages("tidyverse")
  }
  library(tidyverse)

  # init an empty list called data_list
  data_list <- list()
  # # get measures from readcap, qualtrics, task
  # redcap_list <- c("scid","sips_p","sips_d","les", "nsipr", "sips_n", "sips_g")
  # qualtrics_list <- c("demo","lshsr", "rgpts","lec","pdi_40","iipsc", "meim", "pss", "rchat")
  # task_list <- c("kamin","prl","ch","mooney", "social_prl","dsc","eefrt")

  # List all files in the directory
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
      # creates file paths with name to source
      redcap_file <- paste0("clean/redcap/", data_list[i], ".R")
      cat("\n")
      cat(paste("fetching and cleaning", data_list[i], "..."))
      cat("\n")
      # sources each script
      redcap_data <- source(redcap_file)
      #testSuite(data_list[i])
    }
  }
  # source qualtrics cleaning scripts to obtain data frames
  for (i in 1:length(data_list)) {
    if (data_list[i] %in% qualtrics_list) {
      # creates file paths with name to source

      qualtrics_file <- paste0("clean/qualtrics/", data_list[i], ".R")
      # cat(qualtrics_file)
      cat("\n")
      cat(paste("fetching and cleaning", data_list[i], "..."))
      cat("\n")
      # sources each script
      qualtrics_data <- source(qualtrics_file)
      #testSuite(data_list[i])
    }
  }
  # source task cleaning scripts to obtain data frames
  for (i in 1:length(data_list)) {
    if (data_list[i] %in% task_list) {
      # creates file paths with name to source
      task_file <- paste0("clean/task/", data_list[i], ".R")
      cat("\n")
      cat(paste("fetching and cleaning", data_list[i], "..."))
      cat("\n")
      # sources each script
      task_data <- source(task_file)
      #testSuite(data_list[i])


      # task_data <- tryCatch({
      #   exists(task_data)
      # }, warning = function() {
      #   print("does not exist")
      # },finally = {
      #   task_data <- source(task_file)
      # }
      # )
    }
  }

  # Clean Up
  suppressWarnings(source("api/env/cleanup.R"))
}

nda_required_variables <- c("src_subject_id", "phenotype", "site", "visit", "subjectkey", "sex")



# example:
dataRequest("prl")

# requestData("rgpts", "kamin")
# 
