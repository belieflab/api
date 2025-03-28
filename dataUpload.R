dataUpload <- function(data, collection, identifier = "src_subject_id") {
  
  # Ensure the required packages are loaded
  if (!require(config)) {install.packages("config")}; library(config)
  
  if (!require(mongolite)) {install.packages("mongolite")}; library(mongolite)
  
  # Load configuration for MongoDB connection
  config <- config::get()
  
  # Check if the secrets file exists for MongoDB connection string
  if (!file.exists("secrets.R")) message("secrets.R file not found, please create it and add connectionString")
  base::source("secrets.R")
  
  # MongoDB connection setup using config and the specified collection
  db <- config$study_alias
  
  # Connect to the specified collection without retryable writes
  mongo_conn <- mongolite::mongo(
    collection = collection,
    db = db,
    url = connectionString,
    verbose = TRUE,
    options = ssl_options(weak_cert_validation = TRUE, key = "rds-combined-ca-bundle.pem")
  )
  
  # Upload each row as a JSON document
  for (i in seq_len(nrow(data))) {
    row_list <- as.list(data[i, ])
    
    if (!is.null(identifier) && identifier %in% colnames(data)) {
      # Create a query to find an existing document with the same identifier
      query <- sprintf('{"%s": "%s"}', identifier, data[i, identifier])
      
      # Use $set to update specific fields in the document
      update_data <- list('$set' = row_list)
      
      mongo_conn$update(query = query, update = update_data, upsert = TRUE)
    } else {
      # If no identifier is specified, simply insert the document
      mongo_conn$insert(row_list)
    }
  }
  
  # Close the MongoDB connection
  mongo_conn$disconnect(gc = TRUE)
  
  message("Data upload completed successfully.")
}

# Example usage:
# dataUpload(sentimentr_clean, "features")
