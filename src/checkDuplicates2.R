checkRedcapDuplicates <- function(df) {
  df$duplicates  <- duplicated(df[c("src_subject_id", "redcap_event_name")],  first = TRUE)
  
  #separate the duplicates to their own df
  df_dup_ids  <- subset(df, duplicates == TRUE)[c("src_subject_id", "redcap_event_name")]
  
  #filter only the subject ids that are duplicated to include both iterations
  df_duplicates  <<-  df %>% filter(src_subject_id %in% df_dup_ids & redcap_event_name %in% df_dup_ids)
}

#Need to add an if statement for surveys that have multiple visits!
checkQualitricsDuplicates <- function(df) {
  if (deparse(substitute(df)) == "iipsc" | deparse(substitute(df)) == "pqb" | deparse(substitute(df)) == "demo_fu") {
    df$duplicates  <- duplicated(df[c("src_subject_id", "visit")],  first = TRUE)
    
    #separate the duplicates to their own df
    df_dup_ids  <- subset(df, duplicates == TRUE)[c("src_subject_id", "visit")]
    
    #filter only the subject ids that are duplicated to include both iterations
    df_duplicates  <<-  df %>% filter(src_subject_id %in% df_dup_ids & visit %in% df_dup_ids)
    View(df_duplicates)
  } else {
    df$duplicates  <- duplicated(df$src_subject_id,  first = TRUE)
    
    #separate the duplicates to their own df
    df_dup_ids  <- subset(df, duplicates == TRUE)$src_subject_id
    
    #filter only the subject ids that are duplicated to include both iterations
    df_duplicates  <<-  df %>% filter(src_subject_id %in% df_dup_ids)
    View(df_duplicates)
    message(df_duplicates[c("ResponseId","src_subject_id","interview_date","interview_age","phenotype","sex","site","subjectkey","Finished","Progress")])
  }
}

checkTaskDuplicates <- function(df) {
  df2 <- unique(df[c("src_subject_id", "interview_date")])
  df2$duplicates  <- duplicated(df2$src_subject_id,  first = TRUE)
  
  #separate the duplicates to their own df
  df_dup_ids  <- subset(df2, duplicates == TRUE)$src_subject_id
  
  #filter only the subject ids that are duplicated to include both iterations
  df_duplicates  <<-  df %>% filter(src_subject_id %in% df_dup_ids)
}

removeDuplicates <- function(df) {
  # df$duplicates  <- duplicated(df$src_subject_id, first=TRUE)
  # 
  # # #separate the duplicates to their own df
  # # df_dup_ids  <- subset(df, duplicates == FALSE)$src_subject_id
  # # 
  # # #filter only the subject ids that are duplicated to include both iterations
  # # df_duplicates  <-  df %>% filter(src_subject_id %in% df_dup_ids)
  # 
  # df_duplicates <- df[!duplicated(df)]
  
  return (df[!duplicated(df[c('src_subject_id')]),])
}



checkMissingNDAvariables <- function(df) {
  
  message(sapply(df, function(x) sum(is.na(x))))
  
  ##Identifying which rows have missing data 
  message("attention check")
  message(which(is.na(df$ATTN10)))
  message("interview age")
  message(which(is.na(df$interview_age)))
  message("subject key")
  message(which(is.na(df$subjectkey)))
  message("site")
  message(which(is.na(df$site)))
  message("sex")
  message(which(is.na(df$sex)))
  message("phenotype")
  which(is.na(df$phenotype))
}