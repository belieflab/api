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
#' @param chunk_size The number of records to fetch in each parallel batch.
#' @param identifier Field to filter documents by existence and non-emptiness. 
#'        Defaults to "src_subject_id".
#' @return A data.frame consolidating all fetched records.
#' @examples
#' results <- getTask("prl", "workerId", 1000)
#' @export
getMongo <- function(collection_name, db_name = NULL, identifier = "src_subject_id", chunk_size = 10000) {
  start_time <- Sys.time()
  
  # # check to see if identifier is acceptable
  # accepted_identifiers <- c("src_subject_id", "workerId", "PROLIFIC_PID", "participantId, rat_id")
  # 
  # # if identifer supplied by parameter is in the list of accepted identifiers, continue
  # # guard clause
  # if (identifier %!in% accepted_identifiers) {
  #   stop(paste0(identifier, " is not an approved identifier."))
  # }
  
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  Mongo <- Connect(collection_name, db_name)
  config <- config::get()
  if (is.null(db_name)) {
    db_name = config$study_alias
  }
  total_records <- Mongo$count(sprintf('{"%s": {"$ne": ""}}', identifier))
  message(sprintf("Imported %d records from %s/%s. Simplifying into dataframe...", total_records, db_name, collection_name))
  
  # Adjust the initialization of the progress bar to be conditional
  num_chunks <- ceiling(total_records / chunk_size)
  pb <- NULL  # Initialize pb as NULL indicating that the progress bar is not yet created
  
  chunks <- lapply(0:(num_chunks - 1), function(i) {
    list(start = i * chunk_size, size = chunk_size)
  })
  
  num_cores <- parallel::detectCores(logical = TRUE)
  plan(future::multisession, workers = max(1, num_cores - 2))
  if (num_cores > 2) {
    message(sprintf("Speeding up with %d cores!", num_cores - 2))
  }
  
  future_results <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    if (is.null(pb)) {  # Initialize the progress bar when the first chunk starts processing
      pb <- initializeLoadingAnimation(num_chunks + 1)  # +1 for the data combination step
    }
    future_results[[i]] <- future({
      Mongo <- Connect(collection_name, db_name)
      data_chunk <- getData(Mongo, identifier, chunks[[i]])
      data_chunk  # Return the fetched data chunk
    })
    updateLoadingAnimation(pb, i)  # Update the progress bar after each chunk is scheduled
  }
  
  # Collect results and bind them into one dataframe
  results <- lapply(future_results, value)
  df <- dplyr::bind_rows(results)
  
  # Final step in data processing, update the progress bar accordingly
  updateLoadingAnimation(pb, num_chunks + 1)  # Reflect the data combination process
  
  clean_df <- dataHarmonization(df, identifier, collection_name)
  if (!is.null(pb)) {
    completeLoadingAnimation(pb)  # Close the progress bar if it has been created
  }
  cat("\n")  # Ensure the console output starts on a new line after the progress bar
  
  end_time <- Sys.time()
  time_taken <- end_time - start_time
  if (time_taken >= 60) {
    message(sprintf("Data retrieval completed in %.2f minutes", time_taken / 60))
  } else {
    message(sprintf("Data retrieval completed in %.2f seconds", time_taken))
  }
  
  return(clean_df)
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
getData <- function(Mongo, identifier = "src_subject_id", batch_info) {
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
