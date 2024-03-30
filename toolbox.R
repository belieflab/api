toolbox <- function() {
  
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  
  # Optionally, print a message indicating completion or the number of files sourced
  cat("Sourced", length(r_script_files), "R script files from", directory_path, "\n")
  
}

# load all api/src functions into env
toolbox()

