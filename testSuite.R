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



# measure dependent test:
# use testthat to write unit tests specific to measure criteria (for example, range of acceptable values)





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





# is the text that we want actually in the script of interest? true/false
# we have any() outside of the grepl, because readLines goes line by line, and doesn't put all script
#   contents into one vector element/string
# fixed = TRUE treats our text_to_search as a string rather than an expression to be interpreted











#### 7. similar as 5;
# check for string matching rm(

# Trevor & Minerva, 12/18/23: try to create data frame of testSuite results?

# main testSuite function!!

# script_path is for checking cleaning scripts for specific strings to follow convention

################## Test Suite ##################


testSuite <- function(measure_alias, measure_type, script_path) {
  
  # source all scripts in ./api/test
  lapply(list.files("api/test", pattern = "\\.R$", full.names = TRUE), base::source)
  
  
  # List of NDA required variables
  nda_required_variables <- c("src_subject_id", "phenotype", "site", "arm", "visit", "week", 
                              "subjectkey", "sex", "interview_date", "interview_age", "state")
  
  # test cases encapsulated in functions
  
  checkQualtricsDuplicates(measure_alias, measure_type) # and give allow to View them in a table
  
  cleanDataFrameExists(measure_alias, measure_type) #checkin_clean x
  
  ndaRequiredVariablesExist(measure_alias, measure_type, nda_required_variables) # do Nda req variables exist
  
  checkColumnPrefix(measure_alias, measure_type, nda_required_variables) # checkin_distress
  
  checkInterviewAge(measure_alias) # <240 >860
  
  
  # User input to decide which tests to run
  # Optional unit tests to run; "Do you want to run these extra optional tests?"
  
  # additional_tests <- getUserInput("Run additional tests? y/n")
  # 
  # if (additional_tests == "y") {
  #   data_integrity <- getUserInput("Run data integrity unit tests? y/n")
  #   best_practices <- getUserInput("Run best practices unit tests? y/n")
  #   
  #   # Check if each selected test is present in the list and run the corresponding function
  #   if (data_integrity == "y") {
  #     findTextInScript(script_path, text_to_search = "if(!require")
  #     findTextInScript(script_path, text_to_search = "Collaborators")
  #     findTextInScript(script_path, text_to_search = c("describe(", "table(", "ggplot("))
  #   }
  #   
  #   if (best_practices == "y") {
  #     checkNA(measure_alias)
  #   }
  #   
  # }
  
  
  # ...add additional functions here, making sure they pass in measure_alias and measure_type
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