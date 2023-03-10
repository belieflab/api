source("qualtrics/conf.R")

getSurvey <- function(qualtrics) {
  
  if(!require(qualtRics)) {install.packages("qualtRics")}; library(qualtRics)
  
  # check to see if secrets.R exists; if it does not, create it
  if (!file.exists("secrets.R")) {
    file.create("secrets.R");
    return(print("secrets.R file created, please add apiKey and baseUrl"));
  }
  
  source("secrets.R"); # sensitive info for api key
  
  # store qualtrics API credentials
  
  qualtRics::qualtrics_api_credentials(api_key = apiKey, 
                                         base_url = baseUrl,
                                         install = TRUE,
                                         overwrite = TRUE)
  
  # Your original .Renviron will be backed up and stored in your R HOME directory if needed.
  # Your Qualtrics key and base URL have been stored in your .Renviron.  
  # To use now, restart R or run `readRenviron("~/.Renviron")`
  readRenviron("~/.Renviron")
    
  df <- qualtRics::fetch_survey(surveyID = toString(surveyIds[qualtrics]),
                                 verbose = FALSE,
                                 label = FALSE, # both of these must be set to false to import numeric
                                 convert = FALSE, # both of these must be set to false to import numeric
                                 force_request = TRUE)
  
  return(df)
  
}

getResponseId <- function(qualtrics) {
 
  foo <- qualtrics %>% filter_all(any_vars(. %in% "40707"))
  print(foo[c("ResponseId","src_subject_id","interview_age","phenotype","sex","site","subjectkey","Finished","Progress","StartDate","EndDate")])
  name <- deparse(substitute(qualtrics))
  print(surveyIds[name])
  
}
