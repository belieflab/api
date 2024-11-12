# ./api/getTask.R

if (!require(mongolite)) { install.packages("mongolite") }; library(mongolite)
if (!require(future)) { install.packages("future") }; library(future)
if (!require(future.apply)) { install.packages("future.apply") }; library(future.apply)
if (!require(config)) { install.packages("config") }; library(config)

#' Main Parallel Data Retrieval Function
#'
#' This function orchestrates the parallel retrieval of collections from MongoDB
#' It computes the total records based on a query, divides the work into chunks, 
#' and uses parallel processing to fetch the data.
#'
#' @param collection_name The name of the MongoDB collection.
#' @param db_name The name of the MongoDB database. Default is config$study_alias from config.yml.
#' @param identifier If you want to specify identifiers explicitly at the getTask level instead of config.yml.
#' @param chunk_size The number of records to fetch in each parallel batch.
#' @param identifier Field to filter documents by existence and non-emptiness. 
#'        Defaults to "src_subject_id".
#' @return A data.frame consolidating all fetched records.
#' @examples
#' results <- getTask("prl", "workerId", 1000)
#' @export
getMongo <- function(collection_name, db_name = NULL, identifier = NULL, chunk_size = 10000) {
  start_time <- Sys.time()
  
  # Source dependencies and configuration
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  config <- config::get()
  if (is.null(db_name)) {
    db_name <- config$study_alias
  }
  
  # Validate and get identifier
  super_keys <- config$super_keys
  if (is.null(super_keys) || any(super_keys == "")) {
    stop("No super_keys specified in the config file.")
  }
  
  # Initialize MongoDB connection
  Mongo <- Connect(collection_name, db_name)
  
  # Find valid identifier if not provided
  if (is.null(identifier)) {
    for (key in trimws(strsplit(super_keys, ",")[[1]])) {
      count <- Mongo$count(sprintf('{"%s": {"$exists": true, "$ne": ""}}', key))
      if (count > 0) {
        identifier <- key
        break
      }
    }
  }
  
  if (is.null(identifier)) {
    stop("No valid identifier found in the collection.")
  }
  
  message(sprintf("Using identifier: %s", identifier))
  
  # Optimize chunk size based on available memory
  available_memory <- memory.limit()
  if (available_memory < 4000) { # Less than 4GB
    chunk_size <- min(5000, chunk_size)
    message(sprintf("Limited memory detected. Reducing chunk size to %d", chunk_size))
  }
  
  # Stream data using the new function
  message("Starting data streaming...")
  df <- streamMongoData(Mongo, identifier, chunk_size)
  
  # Harmonize the data
  message("Harmonizing data...")
  clean_df <- dataHarmonization(df, identifier, collection_name)
  
  # Report execution time
  end_time <- Sys.time()
  time_taken <- difftime(end_time, start_time, units = "mins")
  message(sprintf("Data retrieval completed in %.2f minutes", as.numeric(time_taken)))
  
  return(clean_df)
}

#' Memory-efficient data harmonization
#' @param df Data frame to harmonize
#' @param identifier Identifier field
#' @param collection_name Collection name
#' @return Harmonized data frame
dataHarmonization <- function(df, identifier, collection_name) {
  # Add visit column efficiently
  if (!("visit" %in% colnames(df))) {
    df$visit <- "bl"
  } else {
    df$visit[is.na(df$visit) | df$visit == ""] <- "bl"
  }
  
  # Convert dates efficiently
  if ("interview_date" %in% colnames(df)) {
    df$interview_date <- as.Date(df$interview_date, "%m/%d/%Y")
  }
  
  # Add measure column
  df$measure <- collection_name
  
  return(df)
}

#' Stream data from MongoDB in chunks
#' @param Mongo MongoDB connection
#' @param identifier Field to query by
#' @param chunk_size Size of each chunk
#' @param max_retries Number of retries for failed chunks
#' @return Data frame with combined results
streamMongoData <- function(Mongo, identifier, chunk_size = 10000, max_retries = 3) {
  query_json <- sprintf('{"%s": {"$ne": ""}}', identifier)
  total_records <- Mongo$count(query_json)
  
  # Initialize an empty list to store chunk results
  num_chunks <- ceiling(total_records / chunk_size)
  all_chunks <- vector("list", num_chunks)
  failed_chunks <- list()
  
  # Simple progress tracking
  pb <- txtProgressBar(min = 0, max = num_chunks, style = 3)
  
  # Process chunks with retry logic
  for (i in seq_along(all_chunks)) {
    chunk_processed <- FALSE
    retries <- 0
    
    while (!chunk_processed && retries < max_retries) {
      tryCatch({
        # Calculate chunk boundaries
        skip <- (i - 1) * chunk_size
        
        # Fetch chunk with explicit sorting to ensure consistency
        chunk <- Mongo$find(
          query = query_json,
          sort = sprintf('{"%s": 1}', identifier),
          skip = skip,
          limit = chunk_size
        )
        
        # Validate chunk data
        if (!is.null(chunk) && nrow(chunk) > 0) {
          all_chunks[[i]] <- chunk
          chunk_processed <- TRUE
        } else {
          warning(sprintf("Empty chunk received at offset %d", skip))
        }
        
      }, error = function(e) {
        warning(sprintf("Error processing chunk %d: %s", i, e$message))
        retries <- retries + 1
        if (retries >= max_retries) {
          failed_chunks[[length(failed_chunks) + 1]] <- list(
            index = i,
            skip = skip,
            error = e$message
          )
        }
        Sys.sleep(1) # Wait before retry
      })
    }
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  # Report any failed chunks
  if (length(failed_chunks) > 0) {
    warning(sprintf("Failed to process %d chunks", length(failed_chunks)))
  }
  
  # Combine chunks efficiently
  combined_df <- do.call(rbind, all_chunks)
  
  # Verify data integrity
  actual_records <- nrow(combined_df)
  if (actual_records < total_records) {
    warning(sprintf("Expected %d records but got %d", total_records, actual_records))
  }
  
  return(combined_df)
}

# ################ #
# Helper Functions #
# ################ #

#' Setup MongoDB connection
#'
#' Establishes a connection to a MongoDB collection using configurations specified 
#' in the './secrets.R' and './config.yml' files.
#'
#' @param collection_name The name of the collection you want to connect to.
#' @return A mongolite::mongo object representing the connection to the MongoDB collection.
#' @examples
#' Mongo <- Connect("prl_sabotage")
#' @export
Connect <- function(collection_name, db_name) {
  if (!file.exists("secrets.R")) stop("secrets.R file not found. Please create it and add connectionString.")
  source("secrets.R")
  config <- config::get()
  if (is.null(db_name)) {
    db_name = config$study_alias
  }
  options <- ssl_options(weak_cert_validation = TRUE, key = "rds-combined-ca-bundle.pem")
  Mongo <- mongolite::mongo(
    collection = collection_name, 
    db = db_name, 
    url = connectionString,
    verbose = FALSE,  # Set verbose to FALSE to suppress messages
    options = options
  )
  return(Mongo)
}

#' Retrieve Task Data
#'
#' Retrieves data from MongoDB based on the specified batch information and query criteria. 
#' It filters out entries where the specified identifier doesn't exist or is empty.
#'
#' @param Mongo The MongoDB connection object.
#' @param identifier The document field to check for existence and non-emptiness.
#' @param batch_info List containing 'start' and 'size' defining the batch to fetch.
#' @return A data.frame with the filtered data or NULL if no valid data is found or in case of error.
#' @examples
#' df <- getData("task_name", list(start = 0, size = 100), Mongo, "src_subject_id")
#' @export
getData <- function(Mongo, identifier, batch_info) {
  query_json <- sprintf('{"%s": {"$ne": ""}}', identifier)
  
  result <- tryCatch({
    df <- Mongo$find(query = query_json, skip = batch_info$start, limit = batch_info$size)
    
    # Additional filtering to ensure the identifier exists
    df <- df[!is.na(df[[identifier]]) & df[[identifier]] != "", ]
    
    if (nrow(df) == 0) {
      message("\nNo data found. Please verify spelling of db/collection name.")
      return(NULL)  # Return NULL if no valid data after filtering
    }
    
    return(df)
  }, error = function(e) {
    message("Error fetching data: ", e$message)
    return(NULL)  # Return NULL in case of error
  })
  
  return(result)
}


#' Data Harmonization Function
#'
#' This function performs data cleaning and preparation tasks, including handling missing values, 
#' converting date formats, and adding necessary columns. It is tailored for a specific dataset 
#' structure used in psychological or medical research.
#'
#' @param df A data frame containing the data to be harmonized. 
#' @param identifier A string that specifies the unique identifier for the dataset; 
#' it influences how date conversions and subsetting are handled.
#' @param collection_name A string representing the specific collection that needs harmonization.
#'
#' @return A data frame with the harmonized data, including standardized 'visit' column entries, 
#' converted interview dates, and added 'measure' column based on the task.
#'
#' @examples
#' # Assuming 'df' is your dataset, 'src_subject_id' is your identifier, and 'rgpts' is your task:
#' harmonized_data <- dataHarmonization(df, 'src_subject_id', 'task1')
#'
#' @importFrom stats setNames
#' @importFrom base ifelse
#' @importFrom base subset
#' @importFrom base as.Date
#' @export
dataHarmonization <- function(df, identifier, collection_name) {
  
  # Ensure 'visit' column exists and update it as necessary
  if (!("visit" %in% colnames(df))) {
    df$visit <- "bl"  # Add 'visit' column with all values as "bl" if it doesn't exist
  } else {
    df$visit <- ifelse(is.na(df$visit) | df$visit == "", "bl", df$visit)  # Replace empty or NA 'visit' values with "bl"
  }
  
  # capr wants as.numeric
  # if (config$study_alias === "capr") {
  #   df$src_subject_id <- as.numeric(df$src_subject_id)
  # }
  
  # convert dates (from string ("m/d/Y") to date format)
  interview_date_exists <- "interview_date" %in% colnames(df)
  
  if (interview_date_exists) {
    df$interview_date <- as.Date(df$interview_date, "%m/%d/%Y")
  }
  
  # add measure column
  df$measure <- collection_name
  
  return(df)
}

getCollections <- function() {
  
  Mongo <- Connect("foo")
  collections <- Mongo$run('{"listCollections":1,"nameOnly":true}')
  print(collections$cursor$firstBatch$name) # lists collections in database
  
  return(collections$cursor$firstBatch$name)
  
}

#' Alias for 'getTask'
getTask <- getMongo
