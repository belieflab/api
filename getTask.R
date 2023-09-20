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

getTask <- function(task) {
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

  query <- '{"src_subject_id": {"$exists": true}}'
  # query <- '{}' # empty query

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

  # check for visit variable, if not add baseline
  if ("visit" %!in% colnames(df_filtered)) {
    df_filtered$visit <- "bl"
  }

  df_filtered$src_subject_id <- as.numeric(df_filtered$src_subject_id)

  # convert dates
  df_filtered$interview_date <- as.Date(df_filtered$interview_date, "%m/%d/%Y")

  # add measure column
  df_filtered$measure <- task

  # return filtered task dataframe
  suppressWarnings(return(df_filtered))
}

# alias
getMongo <- getTask
