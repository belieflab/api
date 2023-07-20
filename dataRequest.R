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

#https://erdavenport.github.io/R-ecology-lesson/03-loops-and-functions.html

dataRequest <- function(...) {
  
  
  if(!require(tidyverse)) {install.packages("tidyverse")}; library(tidyverse)
  
  data_list <- list(...)
  redcap_list <- c("scid","sips_p","sips_d","les")
  qualtrics_list <- c("demo","lshsr", "rgpts","lec","pdi_40")
  task_list <- c("kamin","prl","ch","mooney", "social_prl")
  
  `%!in%` = Negate(`%in%`)
  
  # first, check that eeach measure is valid
  
  invalid_list <-list()
  
  for (i in 1:length(data_list)) {
    if(data_list[i] %!in% redcap_list && data_list[i] %!in% qualtrics_list && data_list[i] %!in% task_list){
      invalid_list <- c(invalid_list, data_list[i])
    }
  }
  
  for (i in 1:length(invalid_list)) {
    cat(paste("ERROR:",invalid_list[i],"is not a valid measure name!\n"))
  }
  
  cat("Please check to make sure it is spelled correctly and exists in these lists:\n")
  cat("REDCap: ")
  cat(paste0(redcap_list),"\n")
  cat("Qualtrics: ")
  cat(paste0(qualtrics_list),"\n")
  cat("Tasks: ")
  cat(paste0(task_list),"\n")
  
  if (length(invalid_list) > 0) {
    return() # abort routine if any measure names are invalid
  }
  
  # source redcap cleaning scripts to obtain data frames
  for (i in 1:length(data_list)) {
    if(data_list[i] %in% redcap_list){
      # creates file paths with name to source
      redcap_file <- paste0("clean/redcap/complete/",data_list[i],".R")
      cat("\n")
      cat(paste("fetching and cleaning",data_list[i],"..."))
      cat("\n")
      # sources each script
      redcap_data <- source(redcap_file)
    }
  }
  # source qualtrics cleaning scripts to obtain data frames
  for (i in 1:length(data_list)) {
    
    if(data_list[i] %in% qualtrics_list){
      # creates file paths with name to source
     
      qualtrics_file <- paste0("clean/qualtrics/complete/",data_list[i],".R")
      print(qualtrics_file)
      cat("\n")
      cat(paste("fetching and cleaning",data_list[i],"..."))
      cat("\n")
      # sources each script
      qualtrics_data <- source(qualtrics_file)
    }
  }
  # source task cleaning scripts to obtain data frames
  for (i in 1:length(data_list)) {
    
    if(data_list[i] %in% task_list){
      # creates file paths with name to source
      task_file <- paste0("clean/task/complete/",data_list[i],".R")
      cat("\n")
      cat(paste("fetching and cleaning",data_list[i],"..."))
      cat("\n")
      # sources each script
      task_data <- source(task_file)
      
      
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
  
}

#requestData("rgpts", "kamin")