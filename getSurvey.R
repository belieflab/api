surveyIds <- list()

# at Temple
surveyIds[[ "ahrs" ]]  <- "SV_eqy0SUuUWk1kEAJ"
surveyIds[[ "ces_d" ]] <- "SV_4OpY0XYe3mEcQct"
surveyIds[[ "ceq" ]]   <- "SV_2aUspDjlgu8EA1D"
surveyIds[[ "cs" ]]    <- "SV_0cEBHdKIunKIv5z"
surveyIds[[ "covid" ]] <- "SV_bgb2FwHf4iRwCwd"
surveyIds[[ "dpb" ]]   <- "SV_24ra0L12Probjh3"
surveyIds[[ "demo" ]]  <- "SV_9nK9whLLeyk4jMF"
surveyIds[[ "duf" ]]   <- "SV_bNTweX4ssZN5ABD"
surveyIds[[ "ehi" ]]   <- "SV_2oc5KcJfi2Z0d93"
surveyIds[[ "eds" ]]   <- "SV_3f9D1O3S8vLOf0V"
surveyIds[[ "eod" ]]   <- "SV_bf65c9tq2EXWDwV"
surveyIds[[ "fh" ]]    <- "SV_3CU6hGMpOnsdLw1"
surveyIds[[ "lshs" ]]  <- "SV_0NUHDlvVHzA8TxX"
surveyIds[[ "lec" ]]   <- "SV_6mqE6GAouhCn8s5"
surveyIds[[ "mapsr" ]] <- "SV_0iY77b00iqNBE8Z"
surveyIds[[ "mapr" ]]  <- "SV_6KkcxyJHgLAbtch"
surveyIds[[ "meim" ]]  <- "SV_bvZJug8zNHiIA5f"
surveyIds[[ "pss" ]]   <- "SV_3V6JW7EzQYT3CVT"
surveyIds[[ "pdi" ]]   <- "SV_1ZmIWE8MYfQCgmN"
surveyIds[[ "psqi" ]]  <- "SV_71b7QZIIk3OfjCJ"
surveyIds[[ "prime" ]] <- "SV_eP71APWAzNiEhdb"
surveyIds[[ "pqb" ]]   <- "SV_3vJ3MEVSjUczQ7b"
surveyIds[[ "pclc" ]]  <- "SV_4Z5gFDO2tNc3UON"
surveyIds[[ "prs" ]]   <- "SV_8r00z3Wx08jOtKZ"
surveyIds[[ "rgpts" ]] <- "SV_2srRiFqfWSmEYUl"
surveyIds[[ "sps" ]]   <- "SV_cT7Qg4XqTCCQSP3"
surveyIds[[ "stai" ]]  <- "SV_ehsGUZEYNtFSSzz"
surveyIds[[ "trhq" ]]  <- "SV_0HZMF8EhZyGKU1D"
surveyIds[[ "via" ]]   <- "SV_danVcuVzvHVjJ5P"

getSurvey <- function(qualtrics) {
  
  if(!require(qualtRics)) {install.packages("qualtRics")}; library(qualtRics)
  
  # check to see if secrets.R exists; if it does not, create it
  if (!file.exists("secrets.R")) {
    file.create("secrets.R");
    return(print("secrets.R file created, please add connectionString"));
  }
  
  source("secrets.R"); # sensitive info for api key
  
  # return all surveyIds into dataframe
  # surveys <- all_surveys() 
  
  surveyIds <- list()
  
  # at Temple
  surveyIds[[ "ahrs" ]]  <- "SV_eqy0SUuUWk1kEAJ"
  surveyIds[[ "ces_d" ]] <- "SV_4OpY0XYe3mEcQct"
  surveyIds[[ "ceq" ]]   <- "SV_2aUspDjlgu8EA1D"
  surveyIds[[ "cs" ]]    <- "SV_0cEBHdKIunKIv5z"
  surveyIds[[ "covid" ]] <- "SV_bgb2FwHf4iRwCwd"
  surveyIds[[ "dpb" ]]   <- "SV_24ra0L12Probjh3"
  surveyIds[[ "demo" ]]  <- "SV_9nK9whLLeyk4jMF"
  surveyIds[[ "duf" ]]   <- "SV_bNTweX4ssZN5ABD"
  surveyIds[[ "ehi" ]]   <- "SV_2oc5KcJfi2Z0d93"
  surveyIds[[ "eds" ]]   <- "SV_3f9D1O3S8vLOf0V"
  surveyIds[[ "eod" ]]   <- "SV_bf65c9tq2EXWDwV"
  surveyIds[[ "fh" ]]    <- "SV_3CU6hGMpOnsdLw1"
  surveyIds[[ "lshs" ]]  <- "SV_0NUHDlvVHzA8TxX"
  surveyIds[[ "lec" ]]   <- "SV_6mqE6GAouhCn8s5"
  surveyIds[[ "mapsr" ]] <- "SV_0iY77b00iqNBE8Z"
  surveyIds[[ "mapr" ]]  <- "SV_6KkcxyJHgLAbtch"
  surveyIds[[ "meim" ]]  <- "SV_bvZJug8zNHiIA5f"
  surveyIds[[ "pss" ]]   <- "SV_3V6JW7EzQYT3CVT"
  surveyIds[[ "pdi" ]]   <- "SV_1ZmIWE8MYfQCgmN"
  surveyIds[[ "psqi" ]]  <- "SV_71b7QZIIk3OfjCJ"
  surveyIds[[ "prime" ]] <- "SV_eP71APWAzNiEhdb"
  surveyIds[[ "pqb" ]]   <- "SV_3vJ3MEVSjUczQ7b"
  surveyIds[[ "pclc" ]]  <- "SV_4Z5gFDO2tNc3UON"
  surveyIds[[ "prs" ]]   <- "SV_8r00z3Wx08jOtKZ"
  surveyIds[[ "rgpts" ]] <- "SV_2srRiFqfWSmEYUl"
  surveyIds[[ "sps" ]]   <- "SV_cT7Qg4XqTCCQSP3"
  surveyIds[[ "stai" ]]  <- "SV_ehsGUZEYNtFSSzz"
  surveyIds[[ "trhq" ]]  <- "SV_0HZMF8EhZyGKU1D"
  surveyIds[[ "via" ]]   <- "SV_danVcuVzvHVjJ5P"
  
  
  # at NU
  surveyIds[[ "iipsc" ]]   <- "SV_7VwYmFJIp60PuE6"
  surveyIds[[ "demo_fu" ]] <- "SV_bNRw9ieiUrX6Bqm"
  
  # store qualtrics API credentials
  
  if (surveyIds[qualtrics] != "SV_7VwYmFJIp60PuE6" || surveyIds[qualtrics] != "SV_bNRw9ieiUrX6Bqm") {
    readRenviron("~/.Renviron")
    qualtRics::qualtrics_api_credentials(api_key = apiKey, 
                                         base_url = baseUrl,
                                         install = TRUE,
                                         overwrite = TRUE)
  } else {
    readRenviron("~/.Renviron")
    qualtRics::qualtrics_api_credentials(api_key = apiKeyNU, 
                                         base_url = baseUrlNU,
                                         install = TRUE,
                                         overwrite = TRUE)
    
  }

  
  df <- qualtRics::fetch_survey(surveyID = toString(surveyIds[qualtrics]),
                                 verbose = FALSE,
                                 label = FALSE, # both of these must be set to false to import numeric
                                 convert = FALSE, # both of these must be set to false to import numeric
                                 force_request = TRUE)
  
  return(df)
  
}

getResponseId <- function(qualtrics) {
  surveyIds <- list()
  
  # at Temple
  surveyIds[[ "ahrs" ]]  <- "SV_eqy0SUuUWk1kEAJ"
  surveyIds[[ "ces_d" ]] <- "SV_4OpY0XYe3mEcQct"
  surveyIds[[ "ceq" ]]   <- "SV_2aUspDjlgu8EA1D"
  surveyIds[[ "cs" ]]    <- "SV_0cEBHdKIunKIv5z"
  surveyIds[[ "covid" ]] <- "SV_bgb2FwHf4iRwCwd"
  surveyIds[[ "dpb" ]]   <- "SV_24ra0L12Probjh3"
  surveyIds[[ "demo" ]]  <- "SV_9nK9whLLeyk4jMF"
  surveyIds[[ "duf" ]]   <- "SV_bNTweX4ssZN5ABD"
  surveyIds[[ "ehi" ]]   <- "SV_2oc5KcJfi2Z0d93"
  surveyIds[[ "eds" ]]   <- "SV_3f9D1O3S8vLOf0V"
  surveyIds[[ "eod" ]]   <- "SV_bf65c9tq2EXWDwV"
  surveyIds[[ "fh" ]]    <- "SV_3CU6hGMpOnsdLw1"
  surveyIds[[ "lshs" ]]  <- "SV_0NUHDlvVHzA8TxX"
  surveyIds[[ "lec" ]]   <- "SV_6mqE6GAouhCn8s5"
  surveyIds[[ "mapsr" ]] <- "SV_0iY77b00iqNBE8Z"
  surveyIds[[ "mapr" ]]  <- "SV_6KkcxyJHgLAbtch"
  surveyIds[[ "meim" ]]  <- "SV_bvZJug8zNHiIA5f"
  surveyIds[[ "pss" ]]   <- "SV_3V6JW7EzQYT3CVT"
  surveyIds[[ "pdi" ]]   <- "SV_1ZmIWE8MYfQCgmN"
  surveyIds[[ "psqi" ]]  <- "SV_71b7QZIIk3OfjCJ"
  surveyIds[[ "prime" ]] <- "SV_eP71APWAzNiEhdb"
  surveyIds[[ "pqb" ]]   <- "SV_3vJ3MEVSjUczQ7b"
  surveyIds[[ "pclc" ]]  <- "SV_4Z5gFDO2tNc3UON"
  surveyIds[[ "prs" ]]   <- "SV_8r00z3Wx08jOtKZ"
  surveyIds[[ "rgpts" ]] <- "SV_2srRiFqfWSmEYUl"
  surveyIds[[ "sps" ]]   <- "SV_cT7Qg4XqTCCQSP3"
  surveyIds[[ "stai" ]]  <- "SV_ehsGUZEYNtFSSzz"
  surveyIds[[ "trhq" ]]  <- "SV_0HZMF8EhZyGKU1D"
  surveyIds[[ "via" ]]   <- "SV_danVcuVzvHVjJ5P"
  
  
  # at NU
  surveyIds[[ "iipsc" ]]   <- "SV_7VwYmFJIp60PuE6"
  surveyIds[[ "demo_fu" ]] <- "SV_bNRw9ieiUrX6Bqm"
  
  foo <- qualtrics %>% filter_all(any_vars(. %in% "40707"))
  print(foo[c("ResponseId","src_subject_id","interview_age","phenotype","sex","site","subjectkey","Finished","Progress","StartDate","EndDate")])
  name <- deparse(substitute(qualtrics))
  print(surveyIds[name])
  
}
