###
### getTask(task)
### parameter: capr task dataframe
###
getTask <- function(task) {
  
  # installs mongolite if not already installed; load mongolite
  if(!require(mongolite)) {install.packages("mongolite")}; library(mongolite);
  
  # check to see if secrets.R exists; if it does not, create it
  if (!file.exists("secrets.R")) {
    file.create("secrets.R");
    return(print("secrets.R file created, please add connectionString"));
  }
  
  source("secrets.R"); # sensitive info for api key

  # store mongoDB connection credentials
  df <- mongolite::mongo(
    collection = task,
    db = "capr",
    url = connectionString,
    verbose = TRUE,
    options = ssl_options(weak_cert_validation = T, key = "rds-combined-ca-bundle.pem")
  );
  
  # close db connection????
  
  # return task dataframe
  return(df$find(query = '{"subjectkey":{"$exists": true}}'));
  
};

  
getTaskPriorToRefactor <- function(task){

  
  if(!require(mongolite)) {install.packages("mongolite")}; library(mongolite)
  # if(!require(rjson)) {install.packages("rjson")}; library(rjson)
  # if(!require(jsonlite)) {install.packages("jsonlite")}; library(jsonlite)
  
  
  # sensitive info for api key
  
  # if (!file.exists("secrets.R")) {print("secrets.R file created, please add apiKey add baseUrl")}; file.create("secrets.R")
  # store mongoDB connection credentials
  source("secrets.R")
  
  if(task == "ch") {  
    
    ch <- mongolite::mongo(
      collection = "ch",
      db = "capr",
      url = connectionString,
      verbose = FALSE,
      options = ssl_options()
    )
    
    return(ch$find(query = '{"subjectkey":{"$exists": true}}', fields = '{"_id" : 0, "stimulus" : 0, "trial_type" : 0}'))
  }
    
  if (task == "dd") {
    dd <- mongolite::mongo(
      collection = "dd",
      db = "capr",
      url = connectionString,
      verbose = FALSE,
      options = ssl_options()
    )
    return(dd$find(query = '{subjectkey:{$exists:true}}'))
  }
  
  if (task == "dsc") {  
    dsc <- mongolite::mongo(
      collection = "dsc",
      db = "capr",
      url = connectionString,
      verbose = FALSE,
      options = ssl_options()
    )
    return(dsc$find(query = '{"subjectkey":{"$exists": true}}'))
    }
  
  if (task == "ebbinghaus") {
    ebbinghaus <- mongolite::mongo(
      collection = "ebbinghaus",
      db = "capr",
      url = connectionString,
      verbose = FALSE,
      options = ssl_options()
    )
    return(ebbinghaus$find(query = '{"subjectkey":{"$exists": true}}'))
  }
  
  
  if (task == "eefrt") {
    eefrt <- mongolite::mongo(
      collection = "eefrt",
      db = "capr",
      url = connectionString,
      verbose = FALSE,
      options = ssl_options()
    )
    return(eefrt$find(query = '{"subjectkey":{"$exists": true}}'))
    }
  
  if (task == "iaps") {
    iaps <- mongolite::mongo(
      collection = "iaps",
      db = "capr",
      url = connectionString,
      verbose = TRUE,
      options = ssl_options()
    )
    return(iaps$find(query = '{"subjectkey":{"$exists": true}}'))
    }
    
  if (task == "kamin") {
    kamin <- mongolite::mongo(
      collection = "kamin",
      db = "capr",
      url = connectionString,
      verbose = FALSE,
      options = ssl_options()
    )
    return(kamin$find(query = '{"subjectkey":{"$exists": true}}'))
    }
  
    
  if (task == "mooney") {
    mooney <- mongolite::mongo(
      collection = "mooney",
      db = "capr",
      url = connectionString,
      verbose = FALSE,
      options = ssl_options()
    )
    return(mooney$find(query = '{"subjectkey":{"$exists": true}}'))
    }
  
    
  if (task == "pessiglione") {
    pessiglione <- mongolite::mongo(
      collection = "pessiglione",
      db = "capr",
      url = connectionString,
      verbose = FALSE,
      options = ssl_options()
    )
    return(pessiglione$find(query = '{"subjectkey":{"$exists": true}}'))
    }
  
  if (task == "prl") {
    prl <- mongolite::mongo(
      collection = "prl",
      db = "capr",
      url = connectionString,
      verbose = FALSE,
      options = ssl_options()
    )
    return(prl$find(query = '{"subjectkey":{"$exists": true}}'))
    
  
    }
    
  if (task == "speed_tap") {
    speed_tap <- mongolite::mongo(
      collection = "speed_tap",
      db = "capr",
      url = connectionString,
      verbose = FALSE,
      options = ssl_options()
    )
    return(speed_tap$find(query = '{"subjectkey":{"$exists": true}}'))
    
    }
    
  if (task == "sws") {
    
    sws <- mongolite::mongo(
      collection = "sws",
      db = "capr",
      url = connectionString,
      verbose = FALSE,
      options = ssl_options()
    )
     return(sws$find(query = '{"subjectkey":{"$exists": true}}'))
    
    }
  
  if (task == "tempo_tap") {
    tempo_tap <- mongolite::mongo(
      collection = "tempo_tap",
      db = "capr",
      url = connectionString,
      verbose = FALSE,
      options = ssl_options()
    )
    return(tempo_tap$find(query = '{"subjectkey":{"$exists": true}}'))
    }
  
  if (task == "var_tap") {
    var_tap <- mongolite::mongo(
      collection = "var_tap",
      db = "capr",
      url = connectionString,
      verbose = FALSE,
      options = ssl_options()
    )
    return(var_tap$find(query = '{"subjectkey":{"$exists": true}}'))
  }
}

# eefrt       <- eefrt$find(query = '{$and: [{subjectkey:{$ne:""}}, {subjectkey:{$exists:true}}]}')






