#' Retrieve Survey Data from Qualtrics
#'
#' This function connects to the Qualtrics API to retrieve survey data based on a specified alias. 
#' It ensures that the correct credentials are used and performs data harmonization before returning the cleaned data frame.
#' 
#' @param qualtrics_alias The alias for the Qualtrics survey to be retrieved.
#' @param identifier The unique identifier for the survey respondents (default is "src_subject_id").
#' @param label Logical indicating whether to return coded values or their associated labels (default is FALSE).
#' @return A cleaned and harmonized data frame containing the survey data.
#' @importFrom dplyr %>% select mutate
#' @export
#' @examples
#' survey_data <- getSurvey("rgpts")
getQualtrics <- function(qualtrics_alias, label = FALSE) {
  # Load required packages
  if (!require(config)) {install.packages("config"); library(config)}
  if (!require(qualtRics)) {install.packages("qualtRics"); library(qualtRics)}
  if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
  
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  
  config <- config::get()
  
  # Get super_keys from config
  super_keys <- config$super_keys
  if (is.null(super_keys) || super_keys == "") {
    stop("No super_keys specified in the config file.")
  }
  
  # Split super_keys if it's a comma-separated string
  if (is.character(super_keys)) {
    super_keys <- strsplit(super_keys, ",")[[1]]
  }
  
  message(ifelse(label, "Extracting choice text:", "Extracting numeric values:"))
  
  # Connect to Qualtrics to ensure correct credentials are used
  connectQualtrics(qualtrics_alias)  # Always connect to ensure correct credentials
  
  show_loading_animation()
  
  df <- getQualtricsData(qualtrics_alias, label)
  if (is.null(df)) {
    stop("Failed to fetch data from Qualtrics.")
  }
  
  # Find the first valid identifier from super_keys
  identifier <- NA
  for (key in super_keys) {
    key <- trimws(key)  # Remove any leading/trailing whitespace
    if (key %in% names(df)) {
      identifier <- key
      break
    }
  }
  
  if (is.na(identifier)) {
    stop("No valid identifier found in the Qualtrics data based on super_keys from config.")
  }
  
  message(sprintf("Using identifier: %s", identifier))
  
  clean_df <- dataHarmonization(df, identifier, qualtrics_alias)
  
  return(clean_df)
}
### mkp dynamic identifier selection
# # dynamically selects identifier from df; if none found, default is src_subject_id
# getQualtrics <- function(qualtrics_alias, label = FALSE) {
#   if (!require(config)) {install.packages("config"); library(config)}
#   if (!require(qualtRics)) {install.packages("qualtRics"); library(qualtRics)}
#   if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
#   
#   lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
#   
#   message(ifelse(label, "Extracting choice text:", "Extracting numeric values:"))
#   
#   connect(qualtrics_alias)
#   show_loading_animation()
#   
#   df <- getQualtricsData(qualtrics_alias, label)
#   
#   super_keys <- c("participantId", "workerId", "PROLIFIC_PID", "src_subject_id")
#   selected_identifier <- intersect(super_keys, colnames(df))
#   
#   if (length(selected_identifier) == 0) {
#     stop("No valid identifier column found.")
#   } else {
#     selected_identifier <- selected_identifier[1]  # Select the first valid identifier
#   }
#   
#   clean_df <- dataHarmonization(df, selected_identifier, qualtrics_alias)
#   
#   return(clean_df)
# }


# ################ #
# Helper Functions #
# ################ #

#' Connect to Qualtrics API
#'
#' This helper function sets up the connection to the Qualtrics API using credentials stored in a file or environment variables. 
#' It is called internally by the 'getSurvey' function.
#' 
#' @param qualtrics_alias The alias for the Qualtrics survey to connect to.
#' @importFrom config get
#' @import qualtRics qualtrics_api_credentials
#' @noRd
connectQualtrics <- function(qualtrics_alias) {
  
  # Validate secrets
  base::source("api/SecretsEnv.R")
  validate_secrets("qualtrics")
  
  config <- config::get()
  
  if (is.null(config$qualtrics$survey_ids)) {
    stop("Survey IDs configuration not found.")
  }
  base::source(config$qualtrics$survey_ids)
  
  if (!(qualtrics_alias %in% names(surveyIds))) {
    stop("Provided qualtrics_alias does not match any survey IDs.")
  }
  
  if (!exists("apiKeys") || !exists("baseUrls")) {
    stop("apiKeys and/or baseUrls arrays not found in secrets.R")
  }
  if (length(apiKeys) != length(baseUrls)) {
    stop("apiKeys and baseUrls arrays must have the same length")
  }
  
  for (i in seq_along(apiKeys)) {
    tryCatch({
      qualtRics::qualtrics_api_credentials(
        api_key = apiKeys[i],
        base_url = baseUrls[i],
        install = TRUE,
        overwrite = TRUE
      )
      return(TRUE)
    }, error = function(e) {
      if (i == length(apiKeys)) {
        stop("Failed to connect with any credentials")
      }
    })
  }
}

#' Retrieve Data from Qualtrics
#'
#' Fetches survey data from Qualtrics based on the survey alias and label preference. 
#' It attempts to fetch survey data and handle any errors that occur.
#'
#' @param qualtrics_alias The alias for the Qualtrics survey whose data is to be fetched.
#' @param label Logical indicating whether to fetch choice labels instead of coded values.
#' @return Data frame containing survey data, or NULL in case of error.
#' @importFrom qualtRics fetch_survey
#' @noRd
getQualtricsData <- function(qualtrics_alias, label) {
  tryCatch({
    config <- config::get()
    base::source(config$qualtrics$survey_ids)
    df <- qualtRics::fetch_survey(
      surveyID = toString(surveyIds[qualtrics_alias]),
      verbose = FALSE,
      label = label,
      convert = label,
      force_request = TRUE,
      add_column_map = TRUE
    )
    if (!is.data.frame(df)) {
      stop(paste("fetch_survey did not return a data frame for", qualtrics_alias))
    }
    return(df)
  }, error = function(e) {
    message("Error in getQualtricsData: ", e$message)
    return(NULL)
  })
}

#' Harmonize Data
#'
#' Performs data cleaning and harmonization on the fetched Qualtrics survey data.
#'
#' @param df Data frame containing Qualtrics survey data.
#' @param identifier The unique identifier for survey respondents.
#' @param qualtrics_alias The alias for the Qualtrics survey.
#' @return Harmonized data frame.
#' @importFrom dplyr mutate
#' @noRd
dataHarmonization <- function(df, identifier, qualtrics_alias) {
  if (!is.data.frame(df)) {
    stop("Input to dataHarmonization is not a data frame")
  }
  
  # check for visit variable, if not add baseline
  if ("visit" %!in% colnames(df)) {
    df$visit <- "bl"
  }
  
  # if visit variable exists, make sure they are named according to convention
  if ("visit" %in% colnames(df)) {
    df$visit <- ifelse(is.na(df$visit), "bl", ifelse(df$visit == "0", "bl", ifelse(df$visit == "12", "12m", ifelse(df$visit == "24", "24m", df$visit))))
  }
  
  # df$src_subject_id <- as.numeric(df$src_subject_id)
  
  # convert dates
  # df$interview_date <- as.Date(df$interview_date, "%m/%d/%Y")
  
  # add measure column
  df$measure <- qualtrics_alias
  
  # select visit
  # df <- df[df$visit==visit,]
  
  suppressWarnings(return(df))
}

#' Extract Column Mapping from Qualtrics Data Frame
#'
#' This function extracts column mappings from the metadata of a Qualtrics survey data frame.
#'
#' @param qualtrics_df Data frame obtained from Qualtrics.
#' @return A list containing the mappings of column names to survey questions.
#' @noRd
getDictionary <- function(qualtrics_df) {
  return(qualtRics::extract_colmap(respdata = qualtrics_df))
}

#' Alias for 'getSurvey'
#'
#' This is an alias for the 'getSurvey' function for ease of use.
#'
#' @inheritParams getSurvey
#' @inherit getSurvey return
#' @export
#' @examples
#' survey_data <- getQualtrics("your_survey_alias")
#' @aliases getQualtrics getSurvey
getSurvey <- getQualtrics
