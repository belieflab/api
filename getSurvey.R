# Get full file paths of all R files in the api directory
# base::source all files using lapply()

# Call the loading animation function before fetch_survey
show_loading_animation <- function() {
  cat("Loading ")
  pb <- txtProgressBar(min = 0, max = 20, style = 3)
  
  for (i in 1:20) {
    Sys.sleep(0.1) # Simulate some computation time
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
}
show_loading_animation()

getSurvey <- function(qualtrics_alias, identifier = "src_subject_id", label = FALSE) {
  
  if (!require(config)) {install.packages("config")}; library(config)
  if (!require(qualtRics)) {install.packages("qualtRics")}; library(qualtRics)
  if (!require(dplyr)) {install.packages("dplyr")}; library(dplyr)
  
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  
  if (label == FALSE) {
    message(paste0("Extracting numeric values:"))
  }
  
  if (label == TRUE) {
    message(paste0("Extracting choice text:"))
  }
  
  # If credentials are already set, connect!
  if (Sys.getenv("QUALTRICS_API_KEY") %in% c("", NA) && Sys.getenv("QUALTRICS_BASE_URL") %in% c("", NA)) {
    Qualtrics <- Connect()
  }
  
  df <- getData(qualtrics_alias, label)
  
  clean_df <- dataHarmonization(df, identifier, qualtrics_alias)
  
  return(clean_df)
  
}

# ################ #
# Helper Functions #
# ################ #

Connect <- function() {
  if (!file.exists("secrets.R")) message("secrets.R file not found, please create it and add apiKey and baseUrl")
  base::source("secrets.R") # sensitive info for api key
  config <- config::get()
  base::source(config$qualtrics$survey_ids)
  
  !(surveyIds[qualtrics_alias] %in% config$qualtrics$survey_ids)
  
  qualtrics_api_key <- if (surveyIds[qualtrics_alias] %in% config$qualtrics$nu_surveys) apiKey2 else apiKey
  qualtrics_base_url <- if (surveyIds[qualtrics_alias] %in% config$qualtrics$nu_surveys) baseUrl2 else baseUrl
  
  qualtRics::qualtrics_api_credentials(
    api_key = qualtrics_api_key,
    base_url = qualtrics_base_url,
    install = TRUE,
    overwrite = TRUE
  )

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
