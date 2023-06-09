# Get full file paths of all R files in the api directory
# base::source all files using lapply()
lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)

#if(!requireNamespace("config", quietly = FALSE)) {install.packages("config")}; library(config)
#if(!requireNamespace("qualtRics", quietly = FALSE)) {install.packages("qualtRics")}; library(qualtRics)

# Define the loading animation function
show_loading_animation <- function() {
  cat("Loading ")
  pb <- txtProgressBar(min = 0, max = 20, style = 3)
  
  for (i in 1:20) {
    Sys.sleep(0.1)  # Simulate some computation time
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
}

# Define the progress callback function
progress_callback <- function(count, total) {
  setTxtProgressBar(pb, count)  # Update the loading animation
}

# Call the loading animation function before fetch_survey
# show_loading_animation()

getSurvey <- function(qualtrics) {
  
  if(!require(config)) {install.packages("config")}; library(config);
  
  config <- config::get()
  
  if(!require(qualtRics)) {install.packages("qualtRics")}; library(qualtRics);
  
  source(config$qualtrics$survey_ids)
  
  # check to see if secrets.R exists; if it does not, create it
  if (!file.exists("secrets.R")) message("secrets.R file not found, please create it and add apiKey and baseUrl")
  
  # store qualtrics API credentials
  base::source("secrets.R"); # sensitive info for api key
  
  !(surveyIds[qualtrics] %in% config$qualtrics$survey_ids)
  
  qualtrics_api_key <- if (surveyIds[qualtrics] %in% config$qualtrics$nu_surveys) apiKey2 else apiKey
  
  qualtrics_base_url <- if (surveyIds[qualtrics] %in% config$qualtrics$nu_surveys) baseUrl2 else baseUrl
  
  qualtRics::qualtrics_api_credentials(api_key = qualtrics_api_key,
                                       base_url = qualtrics_base_url,
                                       install = TRUE,
                                       overwrite = TRUE)
  
  # Your original .Renviron will be backed up and stored in your R HOME directory if needed.
  # Your Qualtrics key and base URL have been stored in your .Renviron.  
  # To use now, restart R or run `readRenviron("~/.Renviron")`
  # this MUST remain to handle switches between api keys
  readRenviron("~/.Renviron")
  
  pb <- txtProgressBar(min = 0, max = 100, style = 3)  # Create progress bar
  
  df <- qualtRics::fetch_survey(surveyID = toString(surveyIds[qualtrics]),
                                verbose = TRUE,
                                label = FALSE, # both of these must be set to false to import numeric
                                convert = FALSE, # both of these must be set to false to import numeric
                                force_request = TRUE)
  
  # Close the progress bar
  close(pb)
  
  return(df)
  
}

getResponseId <- function(qualtrics,name,GUID) {
  
  foo <- qualtrics %>% filter_all(any_vars(. %in% GUID))
  
  # for some reason this just returns the value "qualtrics"
  # name <- deparse(substitute(qualtrics))
  # print(name)
  # surveyId <- toString(surveyIds[qualtrics]
  # print(surveyId)
  
  # the workaround was to create a new parameter and pass in deparse over the parameter, name
  # R is crazy...
  surveyId <- toString(surveyIds[name])
  print(surveyId)
  
  responseId <- foo$ResponseId
  src_subject_id <- foo$src_subject_id
  interview_age <- foo$interview_age
  phenotype <- foo$phenotype
  sex <- foo$sex
  site <- foo$site
  subjectkey <- foo$subjectkey
  
  print(foo[c("ResponseId","src_subject_id","interview_age","phenotype","sex","site","subjectkey","Finished","Progress")])
  
  if (all(!c("visit", "week") %in% colnames(foo))) {
    write(paste(surveyId, responseId,src_subject_id,interview_age,phenotype,sex,site,subjectkey,sep=','),                                            # Write new line to file
          file = paste0("export/",GUID,".csv"),
          append = TRUE)  }
  
  if ("visit" %in% colnames(foo)) {
    visit <- foo$visit
    write(paste(surveyId, responseId,src_subject_id,interview_age,phenotype,sex,site,subjectkey,visit,sep=','),                                            # Write new line to file
          file = paste0("export/",GUID,".csv"),
          append = TRUE)
  }
  
  if ("week" %in% colnames(foo)) {
    week <- foo$week
    write(paste(surveyId, responseId,src_subject_id,interview_age,phenotype,sex,site,subjectkey,week,sep=','),                                            # Write new line to file
          file = paste0("export/",GUID,".csv"),
          append = TRUE)
  }
  
}

setResponseId <- function(qualtrics,name) {
  
  foo <- qualtrics
  
  # for some reason this just returns the value "qualtrics"
  # name <- deparse(substitute(qualtrics))
  # print(name)
  # surveyId <- toString(surveyIds[qualtrics]
  # print(surveyId)
  
  # the workaround was to create a new parameter and pass in deparse over the parameter, name
  # R is crazy...
  surveyId <- toString(surveyIds[name])
  print(surveyId)
  
  responseId <- foo$ResponseId
  src_subject_id <- foo$src_subject_id
  interview_age <- foo$interview_age
  phenotype <- foo$phenotype
  sex <- foo$sex
  site <- foo$site
  subjectkey <- foo$subjectkey
  
  print(foo[c("ResponseId","src_subject_id","interview_age","phenotype","sex","site","subjectkey","Finished","Progress")])
  
  if (all(!c("visit", "week") %in% colnames(foo))) {
    write(paste(surveyId, responseId,src_subject_id,interview_age,phenotype,sex,site,subjectkey,sep=','),                                            # Write new line to file
          file = paste0("export/",name,".csv"),
          append = TRUE)  }
  
  if ("visit" %in% colnames(foo)) {
    visit <- foo$visit
    write(paste(surveyId, responseId,src_subject_id,interview_age,phenotype,sex,site,subjectkey,visit,sep=','),                                            # Write new line to file
          file = paste0("export/",name,".csv"),
          append = TRUE)
  }
  
  if ("week" %in% colnames(foo)) {
    week <- foo$week
    write(paste(surveyId, responseId,src_subject_id,interview_age,phenotype,sex,site,subjectkey,week,sep=','),                                            # Write new line to file
          file = paste0("export/",name,".csv"),
          append = TRUE)
  }
  
}

createSql <- function(qualtrics) {
  
  
  foo <- qualtrics
  name <- deparse(substitute(qualtrics))
  unlink(paste0("export/",name,".csv"))
  
  
  # for some reason this just returns the value "qualtrics"
  # name <- deparse(substitute(qualtrics))
  # print(name)
  # surveyId <- toString(surveyIds[qualtrics]
  # print(surveyId)
  
  # the workaround was to create a new parameter and pass in deparse over the parameter, name
  # R is crazy...
  surveyId <- toString(surveyIds[name])
  print(surveyId)
  
  responseId <- foo$ResponseId
  src_subject_id <- foo$src_subject_id
  interview_age <- foo$interview_age
  phenotype <- foo$phenotype
  sex <- foo$sex
  site <- foo$site
  subjectkey <- foo$subjectkey
  
  print(foo[c("ResponseId","src_subject_id","interview_age","phenotype","sex","site","subjectkey","Finished","Progress")])
  
  if (all(!c("visit", "week") %in% colnames(foo))) {
    
    write(paste("UPDATE qualtrics INNER JOIN consent ON qualtrics.consent_id=consent.consent_id SET qualtrics.response_id ='", responseId,"' WHERE qualtrics.study_HIC = 'PROJECT00001522' AND consort_id=",src_subject_id," AND anonymous_link like'%",surveyId,"'",";",sep=''),                                            # Write new line to file
          file = paste0("export/",name,".csv"),
          append = TRUE)
  }
  
  if ("visit" %in% colnames(foo)) {
    print("YOOOOO")
    visit <- foo$visit
    #update qualtrics set response_id = '' where study_HIC= and visist = and consortid= and link like
    
    write(paste("UPDATE qualtrics INNER JOIN consent ON qualtrics.consent_id=consent.consent_id SET qualtrics.response_id ='", responseId,"' WHERE qualtrics.study_HIC = 'HIC2000026376' AND visit = ",visit," AND lab_id='",src_subject_id,"' AND anonymous_link like'%",surveyId,"'",";",sep=''),                                            # Write new line to file
          file = paste0("export/",name,".csv"),
          append = TRUE)
  }
  
  if ("week" %in% colnames(foo)) {
    week <- foo$week
    write(paste(surveyId, responseId,src_subject_id,interview_age,phenotype,sex,site,subjectkey,week,sep=','),                                            # Write new line to file
          file = paste0("export/",name,".csv"),
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
    if (nrow(df_duplicates) > 0) {
      View(df_duplicates)
      print("Duplicates detected.\nPlease contact your data admin to remove duplicates from Qualtrics.")
      return(df_duplicates)
    }
  }
  
  if ("visit" %in% colnames(df)) {
    
    df$duplicates  <- duplicated(df[c("src_subject_id", "visit")],  first = TRUE)
    
    #separate the duplicates to their own df
    df_dup_ids  <- subset(df, duplicates == TRUE)[c("src_subject_id", "visit")]
    
    #filter only the subject ids that are duplicated to include both iterations
    df_duplicates  <<-  df %>% filter(src_subject_id %in% df_dup_ids & visit %in% df_dup_ids)
    if (nrow(df_duplicates) > 0) {
      View(df_duplicates)
      print("Duplicates detected.\nPlease contact your data admin to remove duplicates from Qualtrics.")
      return(df_duplicates)
    }
  }
  
  if ("week" %in% colnames(df)) {
    
    df$duplicates  <- duplicated(df[c("src_subject_id", "week")],  first = TRUE)
    
    #separate the duplicates to their own df
    df_dup_ids  <- subset(df, duplicates == TRUE)[c("src_subject_id", "week")]
    
    #filter only the subject ids that are duplicated to include both iterations
    df_duplicates  <<-  df %>% filter(src_subject_id %in% df_dup_ids & week %in% df_dup_ids)
    if (nrow(df_duplicates) > 0) {
      View(df_duplicates)
      print("Duplicates detected.\nPlease contact your data admin to remove duplicates from Qualtrics.")
      return(df_duplicates)
    }
  } 
}

updateResponseIds <- function(qualtrics) {
  
  unlink("export/*")
  
  dups <- checkQualtricsDuplicates(qualtrics)
  
  setResponseId(qualtrics,deparse(substitute(qualtrics)))
  
  createPostmanRunner("export/")
  
}

cleaningRoutine <- function(qualtrics) {
  
  unlink("export/*")
  
  dups <- checkQualtricsDuplicates(qualtrics)
  
  guid_list <- as.list(dups$subjectkey)
  
  for (guid in guid_list) {
    getResponseId(qualtrics,deparse(substitute(qualtrics)),guid)
  }
  
  createPostmanRunner("export/")
  
  createCsv(dups)
  
}

removeQualtricsDuplicates <- function(df) {
  
  if(!require(dplyr)) {install.packages("dplyr")}; library(dplyr);
  
  if (all(!c("visit", "week") %in% colnames(df)) ){
    
    df$duplicates  <- duplicated(df$src_subject_id,  first = TRUE)
    
    #separate the duplicates to their own df
    df_dup_ids  <- subset(df, duplicates == TRUE)$src_subject_id
    
    #filter only the subject ids that are duplicated to include both iterations
    df_duplicates  <<-  df %>% filter(src_subject_id %in% df_dup_ids)
    if (nrow(df_duplicates) > 0) {
      View(df_duplicates)
      no_dups <- removeDuplicates(df)
      cat("Duplicates detected!\nDuplicate rows have been removed for this analysis.\nPlease contact your data admin to reconcile and remove duplicates from Qualtrics.")
      return(no_dups)
    } else {
      cat("No duplicates flagged for removal.")
      return(df)
    }
  }
  
  if ("visit" %in% colnames(df)) {
    
    df$duplicates  <- duplicated(df[c("src_subject_id", "visit")],  first = TRUE)
    
    #separate the duplicates to their own df
    df_dup_ids  <- subset(df, duplicates == TRUE)[c("src_subject_id", "visit")]
    
    #filter only the subject ids that are duplicated to include both iterations
    df_duplicates  <<-  df %>% filter(src_subject_id %in% df_dup_ids & visit %in% df_dup_ids)
    if (nrow(df_duplicates) > 0) {
      View(df_duplicates)
      no_dups <- removeDuplicates(df)
      cat("Duplicates detected!\nDuplicate rows have been removed for this analysis.\nPlease contact your data admin to reconcile and remove duplicates from Qualtrics.")
      return(no_dups)
    } else {
      cat("No duplicates flagged for removal.")
      return(df)
    }
  }
  
  if ("week" %in% colnames(df)) {
    
    df$duplicates  <- duplicated(df[c("src_subject_id", "week")],  first = TRUE)
    
    #separate the duplicates to their own df
    df_dup_ids  <- subset(df, duplicates == TRUE)[c("src_subject_id", "week")]
    
    #filter only the subject ids that are duplicated to include both iterations
    df_duplicates  <<-  df %>% filter(src_subject_id %in% df_dup_ids & week %in% df_dup_ids)
    if (nrow(df_duplicates) > 0) {
      View(df_duplicates)
      no_dups <- removeDuplicates(df)
      cat("Duplicates detected!\nDuplicate rows have been removed for this analysis.\nPlease contact your data admin to reconcile and remove duplicates from Qualtrics.")
      return(no_dups)
    } else {
      cat("No duplicates flagged for removal.")
      return(df)
    }
  } 
}

raceSummary <- function(demographics) {
  # preserve original df
  demo <- demographics
  # select columns of interest
  demo_clean <- tibble::tibble(demographics[, grepl("dem", names(demographics ))],
                                     ethnicity = demographics$dem_hispanic
  )
  
  
  colnames(demo_clean) <- c("dem_race_asian",
                                  "dem_race_alaskanative",
                                  "dem_race_americanindian",
                                  "dem_race_africanamerican",
                                  "dem_race_caucasian",
                                  "dem_race_hawaiian",
                                  "dem_other",
                                  "dem_ethnicity"
  )
  
  # recode hispanic
  demo_clean$dem_ethnicity <- ifelse(demo_clean$dem_ethnicity == 0,"not_hispanic_or_latino",
                                           ifelse(demo_clean$dem_ethnicity == 1, "of_hispanic_or_latino",""))
  
  
  demo_clean$count <- rowSums(as.data.frame(sapply(demo_clean[, grepl("race_", names(demo_clean))], as.numeric)), na.rm = TRUE)
  
  # if count = 1, then grab single race column
  # else if count > 1, then label as "More Than One Race"
  # else label as "Unknown or Not Reported"
  
  race_vector <- c()
  
  for (i in 1:nrow(demo_clean)){
    race_vector[i] <- ifelse(demo_clean[i,]$count == 1,colnames(demo_clean)[1:6][which.max(demo_clean[i,1:6])],
                             ifelse(demo_clean[i,]$count > 1, "more_than_one_race","unknwon_or_not_reported"))
  }
  
  demo_clean$dem_race <- race_vector
  
  # add back in NDA required variables
  demo_clean$src_subject_id <- demo$src_subject_id
  demo_clean$subjectkey <- demo$subjectkey
  demo_clean$interview_age <- demo$interview_age
  demo_clean$interview_date <- demo$interview_date
  demo_clean$ResponseId <- demo$ResponseId
  demo_clean$phenotype <- demo$phenotype
  demo_clean$sex <- demo$sex
  demo_clean$site <- demo$site
  
  return(demo_clean)
  
}

# create alias
getQualtrics <- getSurvey
