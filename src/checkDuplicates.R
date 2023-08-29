checkRedcapDuplicates <- function(df) {
  df$duplicates  <- duplicated(df[c("src_subject_id", "visit")],  first = TRUE)
  
  #separate the duplicates to their own df
  df_dup_ids  <- subset(df, duplicates == TRUE)[c("src_subject_id", "visit")]
  
  #filter only the subject ids that are duplicated to include both iterations
  df_duplicates  <<-  df %>% filter(src_subject_id %in% df_dup_ids & visit %in% df_dup_ids)
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
  
  print(sapply(df, function(x) sum(is.na(x))))
  
  ##Identifying which rows have missing data 
  print("attention check")
  print(which(is.na(df$ATTN10)))
  print("interview age")
  print(which(is.na(df$interview_age)))
  print("subject key")
  print(which(is.na(df$subjectkey)))
  print("site")
  print(which(is.na(df$site)))
  print("sex")
  print(which(is.na(df$sex)))
  print("phenotype")
  which(is.na(df$phenotype))
}