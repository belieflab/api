#' ENVIRONMENT MANAGER
#'
#' Keeps R Environment tidy when using dataRequest()
#'
#' @author Joshua G Kenney <joshua.kenney@yale.edu>
#' @author Minerva K Pappu <minerva.pappu@yale.edu>

# # Function to detach all non-base packages
# detach_all_packages <- function() {
#   # Base packages that should not be detached
#   base_packages <- c("package:stats", "package:graphics", "package:grDevices", 
#                      "package:utils", "package:datasets", "package:methods", "package:base")
#   # Get the list of currently loaded packages
#   currently_loaded_packages <- search()
#   # Determine which packages to detach
#   packages_to_detach <- setdiff(currently_loaded_packages, base_packages)
#   
#   # Detach the packages
#   for (pkg in packages_to_detach) {
#     detach(pkg, character.only = TRUE, unload = TRUE, force = TRUE)
#   }
# }
# 
# # Detach all non-base packages
# detach_all_packages()
# 
# nda variables required for merging
nda_merge_vars <- c("src_subject_id", "subjectkey", "phenotype", "visit", "sex", "site")

# List of objects you want to explicitly keep, now correctly including contents of nda_merge_vars
explicit_keep <- c("createCsv", "createSpss", "createRds","getRedcap","getTask","getSurvey",
                   "dataRequest","processMeasure", "performCleanup", "dataFilter", "dataMerge",
                   "getDictionary", "testSuite", nda_merge_vars)

# Since explicit_keep should directly contain the values, ensure nda_merge_vars are expanded into it
explicit_keep <- unique(c(explicit_keep, nda_merge_vars))  # This combines and deduplicates the items

# Assume all_objects_env is defined as follows:
all_objects <- ls(all.names = TRUE)
all_objects_env <- mget(all_objects)

# Filtering data frames ending with "_clean" and "_dictionary"
data_frames_clean <- names(Filter(function(obj) is.data.frame(obj), all_objects_env))
# data_frames_clean <- Filter(function(name) grepl("(_clean$|_dictionary$)", name), data_frames_clean)
data_frames_clean <- Filter(function(name) grepl("(_clean$)", name), data_frames_clean)

# Combine explicit keep list and data frames ending with "_clean"
keep_objects <- unique(c(explicit_keep, data_frames_clean))  # Ensuring no duplicates

# Remove all other objects except for the ones to keep
rm(list = setdiff(all_objects, keep_objects))

# Clean up, if you no longer need these lists
rm(list = c("keep_objects", "data_frames_clean", "all_objects", "explicit_keep", "all_objects_env"))
