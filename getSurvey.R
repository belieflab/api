

getSurvey <- function(qualtrics_alias, identifier = "src_subject_id", label = FALSE) {
  # Load required packages
  if (!require(config)) {install.packages("config"); library(config)}
  if (!require(qualtRics)) {install.packages("qualtRics"); library(qualtRics)}
  if (!require(dplyr)) {install.packages("dplyr"); library(dplyr)}
  
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  
  message(ifelse(label, "Extracting choice text:", "Extracting numeric values:"))
  
  # Connect to Qualtrics to ensure correct credentials are used
  connect(qualtrics_alias)  # Always connect to ensure correct credentials
  
  show_loading_animation()
  
  
  df <- getData(qualtrics_alias, label)
  
  clean_df <- dataHarmonization(df, identifier, qualtrics_alias)
  
  return(clean_df)
}


# ################ #
# Helper Functions #
# ################ #

connect <- function(qualtrics_alias) {
  if (!file.exists("secrets.R")) {
    stop("secrets.R file not found. Please create it and add apiKey and baseUrl.")
  } else {
    base::source("secrets.R")  # Load API key and base URL from secrets
  }
  
  config <- config::get()
  if (!is.null(config$qualtrics$survey_ids)) {
    base::source(config$qualtrics$survey_ids)
  } else {
    stop("Survey IDs configuration not found.")
  }
  
  if (qualtrics_alias %in% names(surveyIds)) {
    qualtrics_api_key <- if (surveyIds[qualtrics_alias] %in% config$qualtrics$nu_surveys) apiKey2 else apiKey
    qualtrics_base_url <- if (surveyIds[qualtrics_alias] %in% config$qualtrics$nu_surveys) baseUrl2 else baseUrl
  } else {
    stop("Provided qualtrics_alias does not match any survey IDs.")
  }
  
  qualtRics::qualtrics_api_credentials(
    api_key = qualtrics_api_key,
    base_url = qualtrics_base_url,
    install = TRUE,
    overwrite = TRUE
  )
  
  # After setting credentials, manually update the environment variables to ensure they're current
  Sys.setenv(QUALTRICS_API_KEY = qualtrics_api_key)
  Sys.setenv(QUALTRICS_BASE_URL = qualtrics_base_url)
}


getData <- function(qualtrics_alias, label) {
  tryCatch({
    config <- config::get()
    base::source(config$qualtrics$survey_ids)
    df <- qualtRics::fetch_survey(
      surveyID = toString(surveyIds[qualtrics_alias]),
      verbose = FALSE,
      label = label, # both of these must be set to false to import numeric
      convert = label, # both of these must be set to false to import numeric
      force_request = TRUE,
      add_column_map = TRUE
    )
    return(df)
  }, error = function(e) {
    message("Error fetching data: ", e$message)
    return(NULL)  # Return NULL in case of error
  })
  
}

dataHarmonization <- function(df, identifier, qualtrics_alias) {
  
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

getDictionary <- function(qualtrics_df) {
  return(extract_colmap(respdata = qualtrics_df))
}

# create alias
getQualtrics <- getSurvey
