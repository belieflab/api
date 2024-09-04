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
    # It's a directory, so we'll process all text files within it
    files <- list.files(file.path(local_path, basename(remote_path)), pattern = "\\.(txt|csv|log)$", full.names = TRUE, recursive = TRUE)
    result_list <- lapply(files, function(file) {
      tryCatch({
        readLines(file)
      }, error = function(e) {
        cat("Error reading file", file, ":\n")
        cat("- Message:", conditionMessage(e), "\n")
        NULL
      })
    })
    result_list <- Filter(Negate(is.null), result_list)
    return(unlist(result_list))  # Unlist to return a single vector
  } else {
    # It's a single file
    file_path <- file.path(local_path, basename(remote_path))
    if (!file.exists(file_path)) {
      cat("Error: Local file was not created. The remote file may not exist or you may not have permission to access it.\n")
      return(NULL)
    }
    tryCatch({
      content <- readLines(file_path)
      return(content)  # Return content directly
    }, error = function(e) {
      cat("Error reading file:\n")
      cat("- Message:", conditionMessage(e), "\n")
      return(NULL)
    })
  }
}