# rm(apiKey)
# rm(apiKey2)
# rm(baseUrl)
# rm(baseUrl2)
# rm(connectionString)
# rm(token)
# rm(uri)
# rm(progress_callback)
# rm(show_loading_animation)
# rm(removeDuplicates)
# rm(getMongo)
# rm(getTask)
# rm(ndaTemplate)
# rm(ndaTemplate)
# rm(`%!in%`)
# rm(checkMissingNDAvariables)
# rm(checkRedcapDuplicates)
# rm(checkTaskDuplicates)
# rm(checkMissingNDAvariables)
# rm(months_in_a_year)
# rm(checkQualtricsDuplicates)
# rm(surveyIds)
# rm(updateResponseIds)
# rm(getQualtrics)
# rm(getRedcap)
# rm(setResponseId)
# rm(removeQualtricsDuplicates)
# rm(raceSummary)
# rm(getSurvey)
# rm(getResponseId)
# rm(getDictionary)
# rm(getColMap)
# rm(getForms)
# rm(createSql)
# rm(createPostmanRunner)
# rm(cleaningRoutine)
# rm(checkCase)
# rm(getUserInput)
# rm(snakeToCamel)
# rm(toCamelCase)
# rm(main)
# rm(loadPackages)
# rm(rawPooler)

# List of objects you want to explicitly keep
explicit_keep <- c("createCsv", "createSpss", "createRds", "dataRequest", "dataMerge")

# Assume all_objects_env is defined as follows:
all_objects <- ls(all.names = TRUE)
all_objects_env <- mget(all_objects)

# Corrected snippet to filter data frames ending with "_clean"
# Correct approach if all_objects_env is a list of objects
data_frames_clean <- names(Filter(function(obj) is.data.frame(obj), all_objects_env))
data_frames_clean <- Filter(function(name) grepl("_clean$", name), data_frames_clean)



# Combine explicit keep list and data frames ending with "_clean"
keep_objects <- unique(c(explicit_keep, data_frames_clean))

# Remove all other objects except for the ones to keep
rm(list = setdiff(all_objects, keep_objects))

# Optionally, if you want to clear 'all_objects' and 'explicit_keep' variables after cleanup
rm(keep_objects, data_frames_clean, all_objects, all_objects_env)


