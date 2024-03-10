# getTaskParallel.R

if (!require(mongolite)) { install.packages("mongolite") }; library(mongolite)
if (!require(future)) { install.packages("future") }; library(future)
if (!require(future.apply)) { install.packages("future.apply") }; library(future.apply)
if (!require(config)) { install.packages("config") }; library(config)

#' Setup MongoDB connection
#'
#' Establishes a connection to a MongoDB collection using configurations specified 
#' in the 'secrets.R' and 'config' files.
#'
#' @param collection_name The name of the collection you want to connect to.
#' @return A mongolite::mongo object representing the connection to the MongoDB collection.
#' @examples
#' mongo_collection <- setupMongoDB("your_collection_name")
#' @export
setupMongoDB <- function(collection_name) {
  if (!file.exists("secrets.R")) stop("secrets.R file not found. Please create it and add connectionString.")
  source("secrets.R")
  config <- config::get()
  mongo_options <- ssl_options(weak_cert_validation = TRUE, key = "rds-combined-ca-bundle.pem")
  mongo_collection <- mongolite::mongo(
    collection = collection_name, 
    db = config$study_alias, 
    url = connectionString,
    verbose = FALSE,  # Set verbose to FALSE to suppress messages
    options = mongo_options
  )
  return(mongo_collection)
}

#' Retrieve Task Data
#'
#' Retrieves data from MongoDB based on the specified batch information and query criteria. 
#' It filters out entries where the specified identifier doesn't exist or is empty.
#'
#' @param task Not used but reserved for future task-specific processing.
#' @param batch_info List containing 'start' and 'size' defining the batch to fetch.
#' @param mongo_collection The MongoDB connection object.
#' @param identifier The document field to check for existence and non-emptiness.
#' @return A data.frame with the filtered data or NULL if no valid data is found or in case of error.
#' @examples
#' df_filtered <- getTaskData("task_name", list(start = 0, size = 100), mongo_collection, "src_subject_id")
#' @export
getTaskData <- function(task, batch_info, mongo_collection, identifier = "src_subject_id") {
  query_json <- sprintf('{"%s": {"$exists": true}}', identifier)
  tryCatch({
    df_filtered <- mongo_collection$find(query = query_json, skip = batch_info$start, limit = batch_info$size)
    # Additional filtering to ensure the identifier exists
    df_filtered <- df_filtered[!is.na(df_filtered[[identifier]]) & df_filtered[[identifier]] != "", ]
    if (nrow(df_filtered) == 0) {
      return(NULL)  # Return NULL if no valid data after filtering
    }
    return(df_filtered)
  }, error = function(e) {
    message("Error fetching data: ", e$message)
    return(NULL)  # Return NULL in case of error
  })
}

#' Main Parallel Data Retrieval Function
#'
#' This function orchestrates the parallel retrieval of data from a MongoDB collection.
#' It computes the total records based on a query, divides the work into chunks, 
#' and uses parallel processing to fetch the data.
#'
#' @param collection_name The name of the MongoDB collection.
#' @param chunk_size The number of records to fetch in each parallel batch.
#' @param identifier Field to filter documents by existence and non-emptiness. 
#'        Defaults to "src_subject_id".
#' @return A data.frame consolidating all fetched records.
#' @examples
#' results <- getTaskParallel("mooney_test", 10000, "custom_id")
#' @export
getTask <- function(collection_name, identifier = "src_subject_id", chunk_size = 10000) {
  start_time <- Sys.time()
  
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  mongo_collection <- setupMongoDB(collection_name)
  
  total_records <- mongo_collection$count(sprintf('{"%s": {"$exists": true}}', identifier))
  message(sprintf("Imported %d records. Simplifying into dataframe...", total_records))
  show_loading_animation()
  
  num_chunks <- ceiling(total_records / chunk_size)
  data_chunks <- lapply(0:(num_chunks - 1), function(i) {
    list(start = i * chunk_size, size = chunk_size)
  })
  
  num_cores <- parallel::detectCores(logical = TRUE)
  plan(future::multisession, workers = max(1, num_cores - 2))
  
  results <- future_lapply(data_chunks, function(chunk) {
    getTaskData(collection_name, chunk, mongo_collection, identifier)
  })
  
  # Use bind_rows() instead of do.call(rbind, ...)
  combined_results <- dplyr::bind_rows(results)
  
  # combined_results <- do.call(rbind, lapply(results, function(df) {
  #   if (!is.null(df)) return(df) else return(NULL)
  # }))
  
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  message(sprintf("Data retrieval completed in %f seconds", as.numeric(time_taken)))
  
  return(combined_results)
}

