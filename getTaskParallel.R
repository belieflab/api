# Required libraries setup
library(mongolite)
library(future)
library(config)

# Load additional R scripts and check for secrets file
lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
if (!file.exists("secrets.R")) stop("secrets.R file not found. Please create it and add connectionString.")

# Load sensitive info for API key and set up MongoDB connection
source("secrets.R")
config <- config::get()
collection_name <- "mooney_test"

# MongoDB connection setup
mongo_collection <- mongolite::mongo(
  collection = collection_name, 
  db = config$study_alias, 
  url = connectionString,
  verbose = TRUE,
  options = ssl_options(weak_cert_validation = TRUE, key = "rds-combined-ca-bundle.pem")
)

# Define your chunk size and calculate the total number of records
chunk_size <- 10000
total_records <- mongo_collection$count('{"src_subject_id": {"$exists": true}}')
num_chunks <- ceiling(total_records / chunk_size)

# Define the data chunks for parallel processing
data_chunks <- lapply(0:(num_chunks - 1), function(i) {
  list(start = i * chunk_size, size = chunk_size)
})

# Set the plan for parallel execution
num_cores <- parallel::detectCores(logical = TRUE)
plan(future::multisession, workers = max(1, num_cores - 1))

# Parallel data retrieval function
getTaskParallel <- function(task, batch_info) {
  query_json <- '{"src_subject_id": {"$exists": true}}'
  tryCatch({
    df_filtered <- mongo_collection$find(query = query_json, skip = batch_info$start, limit = batch_info$size)
    # Additional filtering step to ensure src_subject_id exists
    df_filtered <- df_filtered[!is.na(df_filtered$src_subject_id) & df_filtered$src_subject_id != "", ]
    if (nrow(df_filtered) == 0) {
      return(NULL)  # Return NULL if no valid data after filtering
    }
    return(df_filtered)
  }, error = function(e) {
    message("Error fetching data: ", e$message)
    return(NULL)  # Return NULL in case of error
  })
}



# Fetch data in parallel
results <- future_lapply(data_chunks, function(chunk) {
  getTaskParallel(collection_name, chunk)
})

# Combine results into a single data frame
combined_results <- do.call(rbind, lapply(results, function(df) {
  if (!is.null(df)) {
    return(df)
  } else {
    return(NULL)  # Skip empty data frames
  }
}))

# Measure time taken
end_time <- Sys.time()
time_taken <- end_time - start_time
print(time_taken)
