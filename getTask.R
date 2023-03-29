###
### getTask(task)
### parameter: capr task dataframe
###

if(!require(config)) {install.packages("config")}; library(config);

config <- config::get()

getTask <- function(task) {
  
  # installs mongolite if not already installed; load mongolite
  if(!require(mongolite)) {install.packages("mongolite")}; library(mongolite);
  
  # check to see if secrets.R exists; if it does not, create it
  if (!file.exists("secrets.R")) {
    file.create("secrets.R");
    return(print("secrets.R file created, please add connectionString"));
  }
  
  source("secrets.R"); # sensitive info for api key

  # store mongoDB connection credentials
  df <- mongolite::mongo(
    collection = task,
    db  = config$studyAlias,
    url = connectionString,
    verbose = TRUE,
    options = ssl_options(weak_cert_validation = T, key = "rds-combined-ca-bundle.pem")
  )
  
  # return task dataframe
  return(df$find(query = '{"subjectkey":{"$exists": true}}'))
  #return(df$find(query = '{}'))
  
  # close db connection
  df$disconnect(gc = TRUE)
  
}