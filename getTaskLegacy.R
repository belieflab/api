###
### getTask(task)
### parameter: capr task dataframe
###

# Get full file paths of all R files in the api directory
# base::source all files using lapply()
lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)

# Define the loading animation function
show_loading_animation <- function() {
  cat("Loading ")
  pb <- txtProgressBar(min = 0, max = 20, style = 3)

  for (i in 1:20) {
    Sys.sleep(0.1) # Simulate some computation time
    setTxtProgressBar(pb, i)
  }

  close(pb)
}

# Define the progress callback function
progress_callback <- function(count, total) {
  setTxtProgressBar(pb, count) # Update the loading animation
}

getTask <- function(task, identifier = "src_subject_id") {
  
  message(paste0("default identifier is ",identifier))
  
  if (!require(config)) {
    install.packages("config")
  }
  library(config)

  config <- config::get()

  # installs mongolite if not already installed; load mongolite
  if (!require(mongolite)) {
    install.packages("mongolite")
  }
  library(mongolite)

  # check to see if secrets.R exists; if it does not, create it
  if (!file.exists("secrets.R")) message("secrets.R file not found, please create it and add connectionString")

  base::source("secrets.R") # sensitive info for api key

  # Create a progress bar
  # pb <- txtProgressBar(min = 0, max = 100, style = 3)  # Create progress bar



  # Close the progress bar
  # close(pb)
  if (tolower(identifier) == "src_subject_id") {
    query <- '{"src_subject_id": {"$exists": true}}'
    #query <- '{"src_subject_id": {"$ne": ""}}'
    # query <- '{"$or": [{"src_subject_id": {"$exists": true}}, {"src_subject_id": {"$ne": ""}}]}'
    
    # query <- '{}' # empty query
  
  }
  
  if (tolower(identifier) == "rat_id") {
    query <- '{"rat_id": {"$exists": true}}'
    # query <- '{}' # empty query
    
  }
  
  if (identifier == "workerId") {
    query <- '{"workerId": {"$exists": true}}'
    # query <- '{}' # empty query
    
  }
  
  if (toupper(identifier) == "PROLIFIC_PID") {
    query <- '{"PROLIFIC_PID": {"$exists": true}}'
    # query <- '{}' # empty query
    
  }
  
  start_time <- Sys.time()
  
  # store mongoDB connection credentials
  df <- mongolite::mongo(
    collection = task,
    db = config$study_alias,
    url = connectionString,
    verbose = TRUE,
    options = ssl_options(weak_cert_validation = TRUE, key = "rds-combined-ca-bundle.pem")
  )

  show_loading_animation()

  # find is necessary to create an unlocked enviornment
  df_filtered <- df$find(query = query)

  # close db connection disabled due to endSessions error
  # df$disconnect(gc = TRUE)

  # Check if 'visit' column exists
  if ("visit" %in% colnames(df_filtered)) {
    # 'visit' exists, now check if it's only filled with empty strings, ignoring NAs
    if (all(df_filtered$visit == "" | is.na(df_filtered$visit), na.rm = TRUE)) {
      # If every non-NA entry is an empty string, replace all with "bl"
      df_filtered$visit <- ifelse(is.na(df_filtered$visit) | df_filtered$visit == "", "bl", df_filtered$visit)
    }
    # If there are non-empty non-NA strings, do nothing (pass)
  } else {
    # 'visit' does not exist, so add it and set all values to "bl"
    df_filtered$visit <- "bl"
  }
  
  

  # df_filtered$src_subject_id <- as.numeric(df_filtered$src_subject_id)

  # convert dates (from string ("m/d/Y") to date format)
  if (tolower(identifier) != "rat_id") {
    df_filtered$interview_date <- as.Date(df_filtered$interview_date, "%m/%d/%Y")
  }
  
  # add measure column
  df_filtered$measure <- task
  
  # in the case of delay discounting, for some reason, src_subject_id are empty strings and we need to do this
  if (tolower(identifier) == "src_subject_id") {
    df_filtered <- subset(df_filtered, src_subject_id != "")
  }

  # return filtered task dataframe
  suppressWarnings(return(df_filtered))
  
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  print(time_taken)
}

# alias
getMongo <- getTask

# print(time_taken)

