# First, install R6 if you don't have it
if (!require(R6)) {
  install.packages("R6"); library(R6)
}

# Define a Secrets class without the static method
SecretsEnv <- R6::R6Class("SecretsEnv",
                          public = list(
                            config_specs = list(
                              redcap = list(
                                required = c("uri", "token"),
                                types = c(uri = "character", token = "character")
                              ),
                              mongo = list(
                                required = c("connectionString"),
                                types = c(connectionString = "character")
                              ),
                              qualtrics = list(
                                required = c("apiKeys", "baseUrls"),
                                types = c(apiKeys = "vector", baseUrls = "vector")
                              ),
                              sql = list(
                                required = c("conn"),
                                types = c(connectionString = "character")
                              )
                            ),
                            
                            initialize = function(secrets_file = "secrets.R") {
                              # Check if secrets file exists
                              if (!file.exists(secrets_file)) {
                                stop(secrets_file, " file not found, please create it with required API variables")
                              }
                              
                              # Source the secrets file
                              base::source(secrets_file)
                            },
                            
                            validate_config = function(api_type) {
                              if (!api_type %in% names(self$config_specs)) {
                                stop("Unknown API type: ", api_type)
                              }
                              
                              specs <- self$config_specs[[api_type]]
                              
                              # Check that required variables exist
                              missing_vars <- specs$required[!base::sapply(specs$required, exists)]
                              if (length(missing_vars) > 0) {
                                stop("Missing required ", api_type, " API variables in secrets.R: ",
                                     base::paste(missing_vars, collapse=", "))
                              }
                              
                              # Check variable types based on expected_type
                              for (type_name in unique(specs$types)) {
                                # Get variables expected to be of this type
                                vars_of_type <- names(specs$types[specs$types == type_name])
                                
                                # Check which ones fail the type check
                                failing_vars <- vars_of_type[base::sapply(vars_of_type, function(var) {
                                  var_value <- base::get(var)
                                  
                                  # Check the appropriate type
                                  !switch(type_name,
                                          "character" = is.character(var_value),
                                          "vector" = is.vector(var_value),
                                          FALSE  # Default case for unknown types
                                  )
                                })]
                                
                                # Report if any variables fail this type check
                                if (length(failing_vars) > 0) {
                                  stop("The following ", api_type, " API variables must be ", type_name,
                                       "s created with ", ifelse(type_name == "vector", "c()", "quotes"),
                                       " in secrets.R: ", base::paste(failing_vars, collapse=", "))
                                }
                                
                                # For character variables, check for empty strings
                                if (type_name == "character") {
                                  empty_vars <- vars_of_type[base::sapply(vars_of_type, function(var) {
                                    var_value <- base::get(var)
                                    nchar(var_value) == 0 || all(trimws(var_value) == "")
                                  })]
                                  
                                  if (length(empty_vars) > 0) {
                                    stop("The following ", api_type, " API variables cannot be empty strings in secrets.R: ",
                                         base::paste(empty_vars, collapse=", "))
                                  }
                                }
                                
                                # For vector variables, check if they're empty
                                if (type_name == "vector") {
                                  empty_vars <- vars_of_type[base::sapply(vars_of_type, function(var) {
                                    var_value <- base::get(var)
                                    length(var_value) == 0
                                  })]
                                  
                                  if (length(empty_vars) > 0) {
                                    stop("The following ", api_type, " API variables cannot be empty vectors in secrets.R: ",
                                         base::paste(empty_vars, collapse=", "))
                                  }
                                }
                              }
                              
                              return(TRUE)
                            }
                          )
)

# Create a wrapper function instead of a static method
validate_secrets <- function(api_type, secrets_file = "secrets.R") {
  secrets <- SecretsEnv$new(secrets_file)
  return(secrets$validate_config(api_type))
}