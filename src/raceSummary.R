# used by clean/qualtrics/demo.R

raceSummary <- function(demo_clean) {
  
  demo_clean$demo_ethnicity <- ifelse(demo_clean$demo_hispanic == 4,"not_hispanic_or_latino",
                                      ifelse(demo_clean$demo_hispanic == 1, "of_hispanic_or_latino",""))
  
  demo_clean$demo_count <- suppressWarnings(rowSums(as.data.frame(sapply(demo_clean[, grepl("race_", names(demo_clean))], as.numeric)), na.rm = TRUE))
  
  # if count = 1, then grab single race column
  # else if count > 1, then label as "More Than One Race"
  # else label as "Unknown or Not Reported"
  
  race_vector <- c()
  
  for (i in 1:nrow(demo_clean)){
    race_vector[i] <- ifelse(demo_clean[i,]$demo_count == 1,colnames(demo_clean)[18:23][which.max(demo_clean[i,18:23])],
                             ifelse(demo_clean[i,]$demo_count > 1, "more_than_one_race","unknown_or_not_reported"))
  }
  
  demo_clean$demo_race <- race_vector
  
  # add back in NDA required variables
  # demo_clean$src_subject_id <- demo$src_subject_id
  # demo_clean$subjectkey <- demo$subjectkey
  # demo_clean$interview_age <- demo$interview_age
  # demo_clean$interview_date <- demo$interview_date
  # demo_clean$ResponseId <- demo$ResponseId
  # demo_clean$phenotype <- demo$phenotype
  # demo_clean$sex <- demo$sex
  # demo_clean$site <- demo$site
  
  return(demo_clean)
  
}