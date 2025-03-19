#' ENVIRONMENT MANAGER
#'
#' Keeps R Environment tidy when using dataRequest()
#'
#' @author Joshua G Kenney <joshua.kenney@yale.edu>
#' @author Minerva K Pappu <minerva.pappu@yale.edu>

# nda variables required for merging
nda_merge_vars <- c("src_subject_id", "subjectkey", "phenotype", "visit", "sex", "site")

# List of objects you want to explicitly keep, including contents of nda_merge_vars
# explicit_keep <- c("createCsv", "createSpss", "createRda", "getRedcap", "getTask", "getSurvey",
#                    "dataRequest", "processMeasure", "performCleanup", "dataFilter", "dataMerge",
#                    "getDictionary", "testSuite", "ndaRequest", "dataRequest", nda_merge_vars)

explicit_keep <- c("createCsv", "createSpss", "createRda", "getRedcap", "getTask", "getSurvey",
                   "dataRequest", "performCleanup", "dataFilter", "dataMerge",
                   "getDictionary", "ndaRequest", "dataRequest", nda_merge_vars)

# Since explicit_keep should directly contain the values, ensure nda_merge_vars are expanded into it
explicit_keep <- base::unique(c(explicit_keep, nda_merge_vars))  # This combines and deduplicates the items

# Get all objects in environment
all_objects <- ls(all.names = TRUE)
all_objects_env <- base::mget(all_objects)

# Check whether the objects in environment are: (1) a data frame, or (2) a list 
# but with all the objects within the list being data.frames
data_frames_clean <- base::names(base::Filter(function(obj) {
  is.data.frame(obj) || (is.list(obj) && all(sapply(obj, is.data.frame)))
}, all_objects_env))

# Filtering data frames ending with "_clean", "_scored", or ending with a two-digit 
# number 01-09 (third criteria for NDA objects)
data_frames_clean <- base::Filter(function(name) grepl("(_clean$)|(_scored$)|(0[1-9]$)", name), data_frames_clean)

# Combine explicit keep list and data frames ending with the desired patterns
keep_objects <- base::unique(c(explicit_keep, data_frames_clean)) # Ensuring no duplicates

# Define variables used for cleanup that should be excluded from the main removal
cleanup_vars <- c("keep_objects", "data_frames_clean", "all_objects", "explicit_keep", "all_objects_env", "nda_merge_vars", "cleanup_vars")

# Remove all other objects except for the ones to keep AND the cleanup variables
to_remove <- base::setdiff(all_objects, c(keep_objects, cleanup_vars))
if (length(to_remove) > 0) {
  rm(list = to_remove)
}

# Now remove the cleanup variables
rm(list = c("keep_objects", "data_frames_clean", "all_objects", "explicit_keep", "all_objects_env", "nda_merge_vars", "cleanup_vars", "to_remove"))