# requirements:

# 1. check if _clean exists (done! cleanDataFrameExists)
# 2. check if _clean has NDA required variables (done! ndaRequiredVariablesExist: "src_subject_id", "phenotype", "site", "visit", "subjectkey", "sex")
# 3. check for duplicates in Qualtrics surveys (done I think? 12/29/23 -mkpappu)
# 4. check if item-level summary scores (column names) begin with measure_alias (done! 1/4/24)
# 5. check if libraries are loaded with CAPR standards: if(!require(dplyr)) {install.packages("dplyr")}; library(dplyr);
# 6. check if attention_check is labeled as measure_alias_attention_check
# 7. check if there is a clean up at the end: rm()
# 8. check if data dictionary dictionary exists for qualtrics and redcap

# nice to have:
# suppress Column specification output in console
# modify message displayed not just "test passed"
# documentation for output scores for measures (what does _clean df give us for scores

# findTextInScript
# search for:
#   "Collaborators" (indicates githook was used; can then contact person/author)
#   "describe(" or "table(" or "ggplot(" (suggests someone looked at descriptive stats or data distribution)
#   "rm("
#   "getDictionary("
#   "checkRedcapDuplicates"
#   "sum("
#   "mean(" or "rowMeans(" or "summarize" or "summarise" (indicates that scales, indices, or summary variables have been computed)
#   "


# measure dependent test:
# use testthat to write unit tests specific to measure criteria (for example, range of acceptable values)
if(!require(tidyverse)) {install.packages("tidyverse")};library(tidyverse)


tests <- 0

# Function to perform the audit on a cleaning script output
cleanDataFrameExists <- function(measure_alias, measure_type) {
  
  # append _clean to the measure in question
  output_df_name <- paste0(measure_alias, "_clean")
  
  # test case 1:
  tryCatch({
    df_clean_exists <- test_that("Clean df exists", {
      
      # Check if the expected output_df_name is created
      df_clean_exists <- testthat::expect_true(exists(output_df_name), 
                                               info = paste("The script did not create '", output_df_name, "' dataframe."))
      
      tests = tests+1
      
    })
  }, error = function(e) {
    message("The script did not create '", output_df_name, "' dataframe.", e$message)
  })
  
  
  
}

ndaRequiredVariablesExist <- function(measure_alias, measure_type) {
  # append _clean to the measure in question
  output_df_name <- paste0(measure_alias, "_clean")
  
  # List of NDA required variables
  nda_req_var <- c("src_subject_id", "phenotype", "site", "visit", "subjectkey", "sex")
  
  # store clean dataframe in df_clean
  df_clean <- base::get(output_df_name)
  
  # Check if the output dataframe contains all NDA required variables
  missing_vars <- setdiff(nda_req_var, colnames(df_clean))
  tryCatch({
    test_that("Check for missing NDA required variables", {
      expect_true(length(missing_vars) == 0, 
                  info = paste("All NDA required variables are not present in '", measure_alias, " please make sure the following variable is present in the clean df: '", missing_vars, "."))
    })
  }, error = function(e) {
    message("All NDA required variables are not present in '", measure_alias, " please make sure the following variable is present in the clean df: '", missing_vars, ".", e$message)
  })
  
}


################## test 3 ##################

#
# checkDuplicates <- function(measure_alias, measure_type) {
#   
#   # append _clean to the measure in question
#   output_df_name <- paste0(measure_alias, "_clean")
#   
#   # store clean dataframe in df_clean
#   df <- base::get(output_df_name)
#   
#   identifier <- "src_subject_id"
#   
#   if (measure_type == "qualtrics") {
#   
#     # create a general identifier
#     if (sum(colnames(df) == identifier)==1) {
#       df$identifier <- df[,identifier]
#     } else {message("please provide a valid identifier: src_subject_id, workerId, PROLIFIC_PID")}
#     
#     
#     if (!require(dplyr)) {
#       install.packages("dplyr")
#     }
#     library(dplyr)
#     
#     # this checks that visit and week do not exist because we do not want to identify these data as duplicates
#     if (all(!c("visit", "week") %in% colnames(df))) {
#       df$duplicates <- duplicated(df$identifier, first = TRUE)
#       
#       # separate the duplicates to their own df
#       df_dup_ids <- subset(df, duplicates == TRUE)$identifier
#       
#       # filter only the subject ids that are duplicated to include both iterations
#       df_duplicates <- df %>% filter(identifier %in% df_dup_ids)
#       
#       test_that("Check for Qualtrics duplicates", {
#         expect_true(nrow(df_duplicates) == 0, 
#                     info = paste("No duplicates detected in '", measure_alias, "'."))
#       })
#       
#     }
#     
#     if ("visit" %in% colnames(df)) {
#       df$duplicates <- duplicated(df[c("identifier", "visit")], first = TRUE)
#       
#       # separate the duplicates to their own df
#       df_dup_ids <- subset(df, duplicates == TRUE)[c("identifier", "visit")]
#       
#       # filter only the subject ids that are duplicated to include both iterations
#       df_duplicates <- df %>% filter(identifier %in% df_dup_ids & visit %in% df_dup_ids)
#       
#       test_that("Check for Qualtrics duplicates", {
#         expect_true(nrow(df_duplicates) == 0, 
#                     info = paste("No duplicates detected in '", measure_alias, "'."))
#       })
#       
#     }
#     
#     if ("week" %in% colnames(df)) {
#       df$duplicates <- duplicated(df[c("identifier", "week")], first = TRUE)
#       
#       # separate the duplicates to their own df
#       df_dup_ids <- subset(df, duplicates == TRUE)[c("identifier", "week")]
#       
#       # filter only the subject ids that are duplicated to include both iterations
#       df_duplicates <- df %>% filter(identifier %in% df_dup_ids & week %in% df_dup_ids)
#       
#       test_that("Check for Qualtrics duplicates", {
#         expect_true(nrow(df_duplicates) == 0, 
#                     info = paste("No duplicates detected in '", measure_alias, "'."))
#       })
#       
#     }
#     
#   }
# }
checkDuplicates <- function(measure_alias, measure_type) {
  
  # append _clean to the measure in question
  output_df_name <- paste0(measure_alias, "_clean")
  
  # store clean dataframe in df_clean
  df <- base::get(output_df_name)
  
  identifier <- "src_subject_id"
  
  if (measure_type == "qualtrics") {
    
    if (!require(dplyr)) {
      install.packages("dplyr")
    }
    library(dplyr)
    
    # Check if identifier column exists, if not, prompt for a valid identifier
    if (!(identifier %in% colnames(df))) {
      stop(paste("Please provide a valid identifier: src_subject_id, workerId, PROLIFIC_PID",
                 "\nDuplicates check skipped for '", measure_alias, "'."))
    }
    
    # Define columns to exclude for duplicate checks
    exclusion_cols <- c("visit", "week")
    cols_to_check <- setdiff(colnames(df), c(identifier, exclusion_cols))
    
    # Check if any columns to compare for duplicates exist
    if (length(cols_to_check) == 0) {
      message("No columns found to check for duplicates apart from 'visit' and 'week'.",
              "\nDuplicates check skipped for '", measure_alias, "'.")
      return(invisible())
    }
    
    df$duplicates <- duplicated(df[, cols_to_check, drop = FALSE], fromLast = FALSE)
    df_duplicates <- df[df$duplicates, ]
    
    tryCatch({
      test_that("Check for Qualtrics duplicates", {
        expect_true(nrow(df_duplicates) == 0, 
                    info = paste("No duplicates detected in '", measure_alias, "'."))
      })
    }, error = function(e) {
      message(paste("No duplicates detected in '", measure_alias, "'."), e$message)
    })
  }
}



################## test 4 ##################
library(testthat)

checkColumnPrefix <- function(measure_alias) {
  
  
  # Construct dataframe name based on measure_alias
  df_name <- paste0(measure_alias, "_clean")
  
  # Fetch the dataframe
  df <- base::get(df_name)
  
  # Define NDA required columns
  nda_required <- c("src_subject_id", "phenotype", "site", "visit", "subjectkey", "sex",
                    "interview_age", "interview_date", "ResponseId")
  
  # Extract non-NDA columns
  non_nda_cols <- setdiff(colnames(df), nda_required)
  # print(non_nda_cols)
  
  tryCatch({
    # Check naming convention for non-NDA columns
    test_that("Check column naming convention", {
      expect_true(all(grepl(paste0("^", measure_alias, "_"), non_nda_cols)), 
                  info = paste("Non-NDA columns in '", df_name, 
                               "' follow the correct naming convention."))
    })
  }, error = function(e) {
    message("Non-NDA columns in '", df_name, 
            "' follow the correct naming convention.", e$message)
  })
}




# can we just use summary()? -mkpappu 1/7/2024; will check later
testSummary <- function() {
  totalTests <- 3
  test_that("Test that all tests are passed", {
    expect_equal(totalTests == 3, 
                 info = paste("All NDA required variables are present in '", measure_alias, "'."))
  })
}


#### 4. check _clean dataframe columns
# step 1: grab all column names (maybe need number of col names? so that we can iterate through 1:length(columns_in_clean_df))
# step 2: grab measure_alias as a string and count num of characters
# step 3: iterate through all column names (strings) and check first num of characters (matching measure alias num characters) matches measure_alias
# step 4: if fails on one column, record in list of invalid col names, and continue through length(columns_in_clean_df)
# step 5: pass or fail test; if fail, report problem col names



#### 5. test for capr standards for loading libraries example: if(!require(dplyr)) {install.packages("dplyr")}; library(dplyr);
# check if an if(!require exists?

findTextInScript <- function(script_path, text_to_search) {
  
  script_contents <- base::readLines(script_path)
  
  # Check if any of the strings are present in the script
  matches_grep <- any(sapply(text_to_search, function(text) any(grepl(text, script_contents, fixed = TRUE))))
  
  # Print the result
  print(matches_grep)
  tryCatch({
    # Create a test that fails only if none of the strings are found
    test_that(paste0("At least one of ", toString(text_to_search), " is present"), {
      expect_true(matches_grep, 
                  info = paste(toString(text_to_search), " don't appear in your script."))
    })
  }, error = function(e) {
    message("FindTextInScript failed: ", e$message)
  })
  
 
}


# is the text that we want actually in the script of interest? true/false
# we have any() outside of the grepl, because readLines goes line by line, and doesn't put all script
#   contents into one vector element/string
# fixed = TRUE treats our text_to_search as a string rather than an expression to be interpreted





checkInterviewAge <- function(measure_alias) {
  # append _clean to the measure in question
  output_df_name <- paste0(measure_alias, "_clean")
  
  # store clean dataframe in df_clean
  df_clean <- base::get(output_df_name)
  
  test_that("Check interview_age >= 144", {
    rows_not_meeting_condition <- df_clean$src_subject_id[df_clean$interview_age < 144]
    
    expect_true(all(df_clean$interview_age >= 144),
                info = paste("All values in 'interview_age' should be >= 144. src_subject_id not meeting condition:", rows_not_meeting_condition))
  })
}




checkNA <- function(measure_alias) {
  output_df_name <- paste0(measure_alias, "_clean")
  df_clean <- base::get(output_df_name)
  
  any_na <- which(is.na(df_clean), arr.ind = TRUE)
  
  test_that("Check for NA values", {
    if (length(any_na) > 0) {
      message("NA values found:")
      for (i in 1:nrow(any_na)) {
        row_idx <- any_na[i, 1]
        col_idx <- any_na[i, 2]
        src_subject_id <- df_clean$src_subject_id[row_idx]
        column_name <- colnames(df_clean)[col_idx]
        message(paste("src_subject_id:", src_subject_id, "- Column:", column_name))
      }
      expect_false(TRUE, 
                   info = paste("NA values found. src_subject_id:", df_clean$src_subject_id[any_na[,1]], "- Column:", colnames(df_clean)[any_na[,2]]))
    } else {
      expect_true(TRUE, info = "No NA values should be present in the dataframe.")
    }
  })
}





#### 7. similar as 5;
# check for string matching rm(

# Trevor & Minerva, 12/18/23: try to create data frame of testSuite results?

# main testSuite function!!

# script_path is for checking cleaning scripts for specific strings to follow convention
testSuite <- function(measure_alias, measure_type, script_path) {
  
  if (!require(testthat)) {install.packages("testthat")}; library(testthat)
  
  # COMMENT IN ONLY FOR TESTING PURPOSES
  # source(paste0("clean/", measure_type, "/", measure_alias, ".R"))
  
  # test cases encapsulated in functions
  # 3 required unit tests below will always run
  checkDuplicates(measure_alias, measure_type)
  
  cleanDataFrameExists(measure_alias, measure_type)
  
  ndaRequiredVariablesExist(measure_alias, measure_type)
  
  # checkColumnPrefix(measure_alias)
  
  # User input to decide which tests to run
  # Optional unit tests to run; "Do you want to run these extra optional tests?"
  additional_tests <- getUserInput("Run additional tests? y/n")
  
  if (additional_tests == "y") {
    data_integrity <- getUserInput("Run data integrity unit tests? y/n")
    best_practices <- getUserInput("Run best practices unit tests? y/n")
    
    # Check if each selected test is present in the list and run the corresponding function
    if (data_integrity == "y") {
      findTextInScript(script_path, text_to_search = "if(!require")
      findTextInScript(script_path, text_to_search = "Collaborators")
      findTextInScript(script_path, text_to_search = c("describe(", "table(", "ggplot("))
    }
    
    if (best_practices == "y") {
      checkInterviewAge(measure_alias)
      checkNA(measure_alias)
    }
    
  }

  

  
  # ...add additional functions here, making sure they pass in measure_alias and measure_type
}

getUserInput <- function(prompt_text) {
  cat(prompt_text)
  user_input <- readline()
  return(user_input)
}


# Call the function with the specific cleaning script name
# auditCleaningScript("lshsr.R")




# ## TESTTHAT for nda variables using expect with length of required_variables list;
# might need to change this. because also it doesn't work as of 12/14/23 at 3:40pm. 
# will return to this, but will work on tests in dataRequest.R, because those also look for nda required vars
# library(testthat)
# 
# # Function to audit cleaning scripts
# audit_script <- function(script_path) {
#   # Run the script
#   source(script_path)
# 
#   # Extract script name without extension
#   measure_alias <- tools::file_path_sans_ext(basename(script_path))
# 
#   test_that(paste0("Check required variables in ", measure_alias, "_clean"), {
#     required_variables <- c("src_subject_id", "phenotype", "site", "visit", "subjectkey", "sex")
#     clean_data_name <- get(paste0(measure_alias, "_clean"))
# 
#     missing_variables <- setdiff(required_variables, colnames(clean_data_name))
# 
#     # If missing variables exist, print a custom message
#     if (length(missing_variables) > 0) {
#       cat(" Missing required variables:", paste(missing_variables, collapse = ", "), "\n")
#     }
# 
#     expect_length(missing_variables, 0)
#   })
# 
# 
# 
# }
# 
# # Example usage: replace 'path_to_your_script' with the actual path to your script
# audit_script("clean/qualtrics/pclc.R")
# 
# 



