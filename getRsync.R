getRsync <- function(server, username, remote_path, local_dir = tempdir(), keyfile = "~/.ssh/id_rsa") {
  base::source("secrets.R")
  # Create a unique subdirectory in the local_dir to avoid conflicts
  local_path <- file.path(local_dir, basename(tempfile()))
  dir.create(local_path, recursive = TRUE)
  
  command <- sprintf("rsync -avz -e 'ssh -i %s' %s@%s:'%s' '%s'", keyfile, username, server, remote_path, local_path)
  
  cat("Executing command:", command, "\n")
  
  result <- system(command, intern = TRUE, ignore.stderr = FALSE)
  
  if (any(grepl("^rsync error:", result))) {
    cat("Error executing rsync command:\n")
    cat(paste(result, collapse = "\n"), "\n")
    return(NULL)
  }
  
  # Check if the remote_path was a file or directory
  if (file.info(file.path(local_path, basename(remote_path)))$isdir) {
    # It's a directory, so we'll process all files within it
    files <- list.files(file.path(local_path, basename(remote_path)), pattern = "\\.(txt|csv|log)$", full.names = TRUE, recursive = TRUE)
    
    # Separate CSV files from other files
    csv_files <- files[grepl("\\.csv$", files, ignore.case = TRUE)]
    other_files <- files[!grepl("\\.csv$", files, ignore.case = TRUE)]
    
    if (length(csv_files) > 0) {
      # Process CSV files
      csv_list <- lapply(csv_files, function(file) {
        tryCatch({
          read.csv(file, stringsAsFactors = FALSE)
        }, error = function(e) {
          cat("Error reading CSV file", file, ":\n")
          cat("- Message:", conditionMessage(e), "\n")
          NULL
        })
      })
      csv_list <- Filter(Negate(is.null), csv_list)
      
      if (length(csv_list) > 0) {
        # Combine all CSV files into a single dataframe
        combined_csv <- do.call(rbind, csv_list)
      } else {
        combined_csv <- NULL
      }
    } else {
      combined_csv <- NULL
    }
    
    # Process other files
    other_result <- lapply(other_files, function(file) {
      tryCatch({
        readLines(file)
      }, error = function(e) {
        cat("Error reading file", file, ":\n")
        cat("- Message:", conditionMessage(e), "\n")
        NULL
      })
    })
    other_result <- Filter(Negate(is.null), other_result)
    other_result <- unlist(other_result)
    
    # Return results
    if (!is.null(combined_csv) && length(other_result) > 0) {
      return(list(csv_data = combined_csv, other_data = other_result))
    } else if (!is.null(combined_csv)) {
      return(combined_csv)
    } else if (length(other_result) > 0) {
      return(other_result)
    } else {
      cat("No valid files found in the directory.\n")
      return(NULL)
    }
  } else {
    # It's a single file
    file_path <- file.path(local_path, basename(remote_path))
    if (!file.exists(file_path)) {
      cat("Error: Local file was not created. The remote file may not exist or you may not have permission to access it.\n")
      return(NULL)
    }
    tryCatch({
      if (grepl("\\.csv$", file_path, ignore.case = TRUE)) {
        content <- read.csv(file_path, stringsAsFactors = FALSE)
      } else {
        content <- readLines(file_path)
      }
      return(content)
    }, error = function(e) {
      cat("Error reading file:\n")
      cat("- Message:", conditionMessage(e), "\n")
      return(NULL)
    })
  }
}