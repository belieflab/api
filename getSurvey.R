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
  
  name <- deparse(substitute(qualtrics))
  
  surveyId <- surveyIds[name]
  
  responseId <- foo$ResponseId
  src_subject_id <- foo$src_subject_id
  interview_age <- foo$interview_age
  phenotype <- foo$phenotype
  sex <- foo$sex
  site <- foo$site
  subjectkey <- foo$subjectkey
  
  print(foo[c("ResponseId","src_subject_id","interview_age","phenotype","sex","site","subjectkey","Finished","Progress")])
  
  if (all(!c("visit", "week") %in% colnames(df))) {
    write(paste(surveyId, responseId,src_subject_id,interview_age,phenotype,sex,site,subjectkey,sep=','),                                            # Write new line to file
          file = paste0("export/",GUID,".csv"),
          append = TRUE)  }
  
  if ("visit" %in% colnames(df)) {
    visit <- foo$visit
    write(paste(surveyId, responseId,src_subject_id,interview_age,phenotype,sex,site,subjectkey,visit,sep=','),                                            # Write new line to file
          file = paste0("export/",GUID,".csv"),
          append = TRUE)
  }
    
  if ("week" %in% colnames(df)) {
    week <- foo$week
    write(paste(surveyId, responseId,src_subject_id,interview_age,phenotype,sex,site,subjectkey,week,sep=','),                                            # Write new line to file
          file = paste0("export/",GUID,".csv"),
          append = TRUE)
  }
  
}

createPostmanRunner <- function(path) {
  unlink("all.csv")
  write("surveyId,ResponseId,src_subject_id,interview_age,phenotype,sex,site,subjectkey", file = "all.csv", append = TRUE)
  for (i in list.files(path)) {
    write(readLines(paste0(path,i)),                                           # Write new line to file
    file = "all.csv",
            append = TRUE)
  }
}

checkQualtricsDuplicates <- function(df) {
  
  if(!require(dplyr)) {install.packages("dplyr")}; library(dplyr);
  
  if (all(!c("visit", "week") %in% colnames(df)) ){
    
    df$duplicates  <- duplicated(df$src_subject_id,  first = TRUE)
    
    #separate the duplicates to their own df
    df_dup_ids  <- subset(df, duplicates == TRUE)$src_subject_id
    
    #filter only the subject ids that are duplicated to include both iterations
    df_duplicates  <<-  df %>% filter(src_subject_id %in% df_dup_ids)
    View(df_duplicates)
    return(df_duplicates)
  }
  
  if ("visit" %in% colnames(df)) {
    
    df$duplicates  <- duplicated(df[c("src_subject_id", "visit")],  first = TRUE)
    
    #separate the duplicates to their own df
    df_dup_ids  <- subset(df, duplicates == TRUE)[c("src_subject_id", "visit")]
    
    #filter only the subject ids that are duplicated to include both iterations
    df_duplicates  <<-  df %>% filter(src_subject_id %in% df_dup_ids & visit %in% df_dup_ids)
    View(df_duplicates)
    return(df_duplicates)
  }
    
    if ("week" %in% colnames(df)) {
      
      df$duplicates  <- duplicated(df[c("src_subject_id", "week")],  first = TRUE)
      
      #separate the duplicates to their own df
      df_dup_ids  <- subset(df, duplicates == TRUE)[c("src_subject_id", "week")]
      
      #filter only the subject ids that are duplicated to include both iterations
      df_duplicates  <<-  df %>% filter(src_subject_id %in% df_dup_ids & week %in% df_dup_ids)
      View(df_duplicates)
      return(df_duplicates)
    } 
}
