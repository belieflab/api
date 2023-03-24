if(!require(config)) {install.packages("config")}; library(config);

config <- config::get()

source(config$surveyIds)

getSurvey <- function(qualtrics) {
  
  if(!require(qualtRics)) {install.packages("qualtRics")}; library(qualtRics)
  
  # check to see if secrets.R exists; if it does not, create it
  if (!file.exists("secrets.R")) {
    file.create("secrets.R");
    return(print("secrets.R file created, please add apiKey and baseUrl"));
  }
  
  source("secrets.R"); # sensitive info for api key
  
  # store qualtrics API credentials
  
  # Your original .Renviron will be backed up and stored in your R HOME directory if needed.
  # Your Qualtrics key and base URL have been stored in your .Renviron.  
  # To use now, restart R or run `readRenviron("~/.Renviron")`
  readRenviron("~/.Renviron")
  
  if (surveyIds[qualtrics] != config$survey1 || surveyIds[qualtrics] != config$survey2) {
    readRenviron("~/.Renviron")
    qualtRics::qualtrics_api_credentials(api_key = apiKey, 
                                         base_url = baseUrl,
                                         install = TRUE,
                                         overwrite = TRUE)
  } else {
    readRenviron("~/.Renviron")
    qualtRics::qualtrics_api_credentials(api_key = apiKey2, 
                                         base_url = baseUrl2,
                                         install = TRUE,
                                         overwrite = TRUE)
    
  }
  
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

getResponseId <- function(qualtrics,GUID) {
  
  foo <- qualtrics %>% filter_all(any_vars(. %in% GUID))
  print(foo[c("ResponseId","src_subject_id","interview_age","phenotype","sex","site","subjectkey","Finished","Progress","visit")])
  responseId <- foo$ResponseId
  src_subject_id <- foo$src_subject_id
  interview_age <- foo$interview_age
  phenotype <- foo$phenotype
  sex <- foo$sex
  site <- foo$site
  subjectkey <- foo$subjectkey
  visit <- foo$visit
  name <- deparse(substitute(qualtrics))
  print(surveyIds[name])
  surveyId <- surveyIds[name]
  write(paste(surveyId, responseId,src_subject_id,interview_age,phenotype,sex,site,subjectkey,visit,sep=','),                                            # Write new line to file
        file = paste0("export/",GUID,".csv"),
        append = TRUE)
  
}


getResponseIdNoVisit <- function(qualtrics,GUID) {
  
  foo <- qualtrics %>% filter_all(any_vars(. %in% GUID))
  print(foo[c("ResponseId","src_subject_id","interview_age","phenotype","sex","site","subjectkey","Finished","Progress")])
  responseId <- foo$ResponseId
  src_subject_id <- foo$src_subject_id
  interview_age <- foo$interview_age
  phenotype <- foo$phenotype
  sex <- foo$sex
  site <- foo$site
  subjectkey <- foo$subjectkey
  name <- deparse(substitute(qualtrics))
  print(surveyIds[name])
  surveyId <- surveyIds[name]
  write(paste(surveyId, responseId,src_subject_id,interview_age,phenotype,sex,site,subjectkey,sep=','),                                            # Write new line to file
        file = paste0("export/",GUID,".csv"),
        append = TRUE)
  
}

createPostmanRunner <- function(path) {
  for (i in list.files(path)) {
    write(readLines(paste0(path,+"/"+i)),                                           # Write new line to file
    file = "all.csv",
            append = TRUE)
  }
}
