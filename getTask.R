if (!require(mongolite)) { install.packages("mongolite") }; library(mongolite)
if (!require(future)) { install.packages("future") }; library(future)
if (!require(future.apply)) { install.packages("future.apply") }; library(future.apply)
if (!require(config)) { install.packages("config") }; library(config)
if (!require(dplyr)) { install.packages("dplyr") }; library(dplyr)

#' Cross-platform memory check function
#' @return Available memory in GB or NULL if unable to determine
getAvailableMemory <- function() {
  tryCatch({
    if (.Platform$OS.type == "windows") {
      # Try using Windows system command
      wmic_cmd <- tryCatch({
        mem_info <- system('wmic OS get FreePhysicalMemory /Value', intern = TRUE)
        # Extract the number from "FreePhysicalMemory=XXXXX"
        mem_line <- grep("FreePhysicalMemory=", mem_info, value = TRUE)
        mem_kb <- as.numeric(sub("FreePhysicalMemory=", "", mem_line))
        return(mem_kb / (1024^2))  # Convert KB to GB
      }, error = function(e) {
        # Fallback to memory.size if wmic fails
        total_mem <- memory.size(max = TRUE)
        if (!is.na(total_mem)) {
          return(total_mem / 1024)  # Convert MB to GB
        }
        return(NULL)
      })
      return(wmic_cmd)
    } else if (Sys.info()["sysname"] == "Darwin") {
      mem_info <- system("sysctl hw.memsize", intern = TRUE)
      if (length(mem_info) > 0) {
        total_mem <- as.numeric(strsplit(mem_info, " ")[[1]][2])
        return(total_mem / (1024^3))
      }
    } else {
      if (file.exists("/proc/meminfo")) {
        mem_info <- readLines("/proc/meminfo")
        mem_free <- grep("MemAvailable:", mem_info, value = TRUE)
        if (length(mem_free) > 0) {
          mem_kb <- as.numeric(strsplit(mem_free, "\\s+")[[1]][2])
          return(mem_kb / (1024^2))
        }
      }
    }
  }, error = function(e) {
    return(NULL)
  })
  return(NULL)
}

#' Calculate optimal resource parameters
#' @param total_records Total number of records to process
#' @return List containing optimal chunk size and number of workers
calculateResourceParams <- function(total_records) {
  # Default/fallback values
  default_params <- list(
    chunk_size = 1000,
    workers = 2
  )
  
  # Try to get system resources
  mem <- getAvailableMemory()
  num_cores <- parallel::detectCores(logical = TRUE)
  
  if (!is.null(mem)) {
    message(sprintf("System resources: %.0fGB RAM, %d-core CPU.", mem, num_cores))
  } else {
    message(sprintf("System resources: %d-core CPU.", num_cores))  # Skip RAM info if unavailable
  }
  
  # If we can't detect resources, return defaults
  if (is.null(mem) || is.null(cores)) {
    warning("Unable to determine system resources. Using conservative defaults.")
    return(default_params)
  }
  
  # Calculate parameters based on available resources
  params <- list()
  
  # Adjust chunk size based on memory
  if (mem < 4) {
    params$chunk_size <- 500  # Very conservative for low memory
  } else if (mem < 8) {
    params$chunk_size <- 1000  # Conservative for moderate memory
  } else if (mem < 16) {
    params$chunk_size <- 2000  # Moderate for good memory
  } else {
    params$chunk_size <- 5000  # Aggressive for high memory
  }
  
  # Adjust workers based on cores
  if (cores <= 2) {
    params$workers <- 1  # Single worker for very limited cores
  } else if (cores <= 4) {
    params$workers <- cores - 1  # Leave 1 core for small systems
  } else {
    params$workers <- cores - 2  # Leave 2 cores for larger systems
  }
  
  # Additional adjustments based on total records
  if (total_records < params$chunk_size * 2) {
    params$chunk_size <- max(500, floor(total_records / 2))
    params$workers <- min(params$workers, 2)
  }
  
  # Removed the duplicate messages from here
  
  return(params)
}

#' Initialize a clean loading animation
#' @param steps Number of steps in the process
#' @return Loading animation object
initializeLoadingAnimation <- function(steps) {
  list(
    steps = steps,
    current = 0,
    width = 50,
    start_time = Sys.time()
  )
}

#' Update the loading animation
#' @param pb Loading animation object
#' @param current Current step
updateLoadingAnimation <- function(pb, current) {
  pb$current <- current
  percentage <- round(current / pb$steps * 100)
  filled <- round(pb$width * current / pb$steps)
  bar <- paste0(
    strrep("=", filled),
    strrep(" ", pb$width - filled)
  )
  cat(sprintf("\r  |%s| %3d%%", bar, percentage))
  utils::flush.console()
}

#' Complete the loading animation
#' @param pb Loading animation object
completeLoadingAnimation <- function(pb) {
  updateLoadingAnimation(pb, pb$steps)
  cat("\n")
}

#' Format duration in a human-friendly way
formatDuration <- function(duration) {
  secs <- as.numeric(duration, units = "secs")
  if (secs < 60) {
    return(sprintf("%.1f seconds", secs))
  } else {
    mins <- floor(secs / 60)
    remaining_secs <- round(secs %% 60, 1)
    if (remaining_secs > 0) {
      return(sprintf("%d minutes and %.1f seconds", mins, remaining_secs))
    } else {
      return(sprintf("%d minutes", mins))
    }
  }
}

#' Main data retrieval function
#' @export
# Previous helper functions remain the same...

getMongo <- function(collection_name, db_name = NULL, identifier = NULL, chunk_size = NULL) {
  start_time <- Sys.time()
  
  # Suppress MongoDB messages globally
  options(mongolite.quiet = TRUE)
  
  # Source dependencies and get config
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  config <- config::get()
  if (is.null(db_name)) {
    db_name <- config$study_alias
  }
  
  # Validate super_keys
  super_keys <- config$super_keys
  if (is.null(super_keys) || any(super_keys == "")) {
    stop("No super_keys specified in the config file.")
  }
  
  # Initialize MongoDB connection
  Mongo <- Connect(collection_name, db_name)
  
  # Find valid identifier
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
  
  # message(sprintf("Using identifier: %s", identifier))
  
  # Get total records
  query_json <- sprintf('{"%s": {"$ne": ""}}', identifier)
  total_records <- Mongo$count(query_json)
  
  # Get and display system resources
  mem <- getAvailableMemory()
  num_cores <- parallel::detectCores(logical = TRUE)
  workers <- max(1, num_cores - 2)
  message(sprintf("System resources: %.0fGB RAM, %d-core CPU.", mem, num_cores))
  
  # Adjust chunk size based on memory
  if (!is.null(mem)) {
    if (mem < 4) {
      chunk_size <- 500
    } else if (mem < 8) {
      chunk_size <- 1000
    } else if (mem < 16) {
      chunk_size <- 2000
    } else {
      chunk_size <- 5000
    }
  } else {
    # Default if memory detection fails
    chunk_size <- 1000  # Conservative default
  }
  
  #message(sprintf("Using chunk size: %d, workers: %d", chunk_size, workers))
  if (workers > 1) {
    message(sprintf("Processing in parallel with %d workers!", workers))
  }
  
  # Setup chunks
  num_chunks <- ceiling(total_records / chunk_size)
  chunks <- createChunks(total_records, chunk_size)
  
  # Setup parallel processing with quiet connections
  plan(future::multisession, workers = workers)
  
  
  
  # Progress message
  #message("Retrieving data:")
  #message(sprintf("Found %d records in %s/%s", total_records, db_name, collection_name))
  message(sprintf("\nImporting %d records from %s/%s using %s. Simplifying into dataframe...", 
                  total_records, db_name, collection_name, identifier))
  
  # Initialize custom progress bar
  pb <- initializeLoadingAnimation(num_chunks)
  
  # Process chunks
  future_results <- vector("list", length(chunks))
  for (i in seq_along(chunks)) {
    future_results[[i]] <- future({
      temp <- tempfile()
      sink(temp)
      on.exit({
        sink()
        unlink(temp)
      })
      
      tryCatch({
        Mongo <- Connect(collection_name, db_name)
        batch_info <- chunks[[i]]
        if (!is.null(batch_info) && !is.null(batch_info$start) && !is.null(batch_info$size)) {
          data_chunk <- getData(Mongo, identifier, batch_info)
        } else {
          warning("Invalid batch info, skipping chunk")
          return(NULL)
        }
        data_chunk
      }, error = function(e) {
        warning(sprintf("Error processing chunk %d: %s", i, e$message))
        NULL
      })
    })
    updateLoadingAnimation(pb, i)
  }
  
  # Collect results
  results <- lapply(future_results, value)
  
  # Combine results
  # message("\nCombining data chunks...")
  df <- dplyr::bind_rows(results)
  completeLoadingAnimation(pb)
  
  # Harmonize data
  message("Harmonizing data...", appendLF = FALSE)  # appendLF=FALSE prevents line feed
  clean_df <- dataHarmonization(df, identifier, collection_name)
  Sys.sleep(0.5)  # Optional: small pause for visual effect
  message("\rHarmonizing data...done.")
  
  # Report execution time
  end_time <- Sys.time()
  duration <- difftime(end_time, start_time, units = "secs")
  Sys.sleep(0.5)  # Optional: small pause for visual effect
  message(sprintf("\nData retrieval completed in %s.", formatDuration(duration-1))) # minus 1 to account for sleep
  
  return(clean_df)
}

# ################ #
# Helper Functions #
# ################ #

createChunks <- function(total_records, chunk_size) {
  tryCatch({
    num_chunks <- ceiling(total_records / chunk_size)
    chunks <- vector("list", num_chunks)
    for (i in seq_len(num_chunks)) {
      chunks[[i]] <- list(
        start = (i - 1) * chunk_size,
        size = if (i == num_chunks) {
          min(chunk_size, total_records - ((i - 1) * chunk_size))
        } else {
          chunk_size
        }
      )
    }
    return(chunks)
  }, error = function(e) {
    warning("Error creating chunks, falling back to single chunk")
    return(list(list(start = 0, size = total_records)))
  })
}

#' Setup MongoDB connection with suppressed messages
#' @param collection_name The name of the collection you want to connect to.
#' @param db_dname The name of the database you cant to connect to.
#' @return A mongolite::mongo object representing the connection to the MongoDB collection.
Connect <- function(collection_name, db_name) {
  if (!file.exists("secrets.R")) stop("secrets.R file not found. Please create it and add connectionString.")
  source("secrets.R")
  config <- config::get()
  if (is.null(db_name)) {
    db_name = config$study_alias
  }
  options <- ssl_options(weak_cert_validation = TRUE, key = "rds-combined-ca-bundle.pem")
  
  # The key is to use sink() to capture and discard the messages
  temp <- tempfile()
  sink(temp)
  on.exit({
    sink()
    unlink(temp)
  })
  
  Mongo <- mongolite::mongo(
    collection = collection_name, 
    db = db_name, 
    url = connectionString,
    verbose = FALSE,
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
