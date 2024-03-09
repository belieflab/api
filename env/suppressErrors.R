# Temporarily redirect STDERR to NULL
suppressMongoMessages <- function() {
  sink(stderr(), type = "message") # Redirect STDERR
}

# Reset STDERR back to console
resetMongoMessages <- function() {
  sink(NULL, type = "message") # Reset redirection
}
