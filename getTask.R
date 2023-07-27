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
    Sys.sleep(0.1)  # Simulate some computation time
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
}

# Define the progress callback function
progress_callback <- function(count, total) {
  setTxtProgressBar(pb, count)  # Update the loading animation
}

getTask <- function(task) {
  
  if(!require(config)) {install.packages("config")}; library(config);

  config <- config::get()
  
  # installs mongolite if not already installed; load mongolite
  if(!require(mongolite)) {install.packages("mongolite")}; library(mongolite);
  
  # check to see if secrets.R exists; if it does not, create it
  if (!file.exists("secrets.R")) message("secrets.R file not found, please create it and add connectionString")
  
  base::source("secrets.R"); # sensitive info for api key
  
  # Create a progress bar
  #pb <- txtProgressBar(min = 0, max = 100, style = 3)  # Create progress bar
  

  
  # Close the progress bar
  #close(pb)
  
  # store mongoDB connection credentials
  df <- mongolite::mongo(
    collection = task,
    db  = config$study_alias,
    url = connectionString,
    verbose = TRUE,
    options = ssl_options(weak_cert_validation = T, key = "rds-combined-ca-bundle.pem")
  )
  
  
  show_loading_animation()
  
  
  # return task dataframe
  return(df$find(query = '{"subjectkey":{"$exists": true}}'))
  # return(df$find(query = '{}'))
  
  # close db connection
  df$disconnect(gc = TRUE)
  
  
  
}

# alias
getMongo <- getTask
