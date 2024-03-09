# Define the loading animation function
show_loading_animation <- function() {
  cat("Loading ")
  pb <- txtProgressBar(min = 0, max = 20, style = 3)
  
  for (i in 1:20) {
    Sys.sleep(0.1) # Simulate some computation time
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
}

# Define the progress callback function
progress_callback <- function(count, total) {
  setTxtProgressBar(pb, count) # Update the loading animation
}