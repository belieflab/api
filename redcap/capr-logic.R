processCaprData <- function(df, instrument_name) {
  # Convert and filter src_subject_id
  df$src_subject_id <- as.numeric(df$src_subject_id)
  # df <- filter(df, between(df$src_subject_id, 10000, 71110)) # between seems() to cause error
  # might be less flexible character to numeric
  # src_subject_id may download as character sometimes
  df <- dplyr::filter(df, src_subject_id > 10000, src_subject_id < 79110)
  # include guard clauses for mesaures that require aditional filtering beyond form name
  if (instrument_name == "scid_scoresheet") {
    df %>% dplyr::select(contains(c("src_subject_id", "redcap_event_name", "scid_", "scip_", "mdd_", "pdd_"))) # scid_p18a was misspelled in the dataframe, that is why there is a "scip" variable :)
  }
  df$src_subject_id <- as.character(df$src_subject_id)
  
  # Close the progress bar
  # close(pb)
  
  # create a visit variable based on redcap_event_name
  ## not over-writing with rename(), so that redcap_event_name can do a "soft retire"
  df %>% dplyr::mutate(visit = redcap_event_name) -> df
  
  # align redcap_event_name-ing convention with more natural language
  df %>% dplyr::mutate(visit = ifelse(visit == "baseline_arm_1", "bl",
                                      ifelse(visit == "12m_arm_1", "12m",
                                             ifelse(visit == "24m_arm_1", "24m", NA)
                                      )
  )) -> df
  
  # recode phenotype (only need to recode phenotypes as 4 (ineligible) and 5 (withdrawn) have been removed in previous line)
  df %>% dplyr::mutate(phenotype = ifelse(is.na(phenotype),NA,
                                          ifelse(phenotype == 1, "hc",
                                                 ifelse(phenotype == 2, "chr",
                                                        ifelse(phenotype == 3, "hsc", 
                                                               ifelse(phenotype == 4, "ineligible",
                                                                      ifelse(phenotype == 5, "withdrawn",NA))))))) -> df
  
  #make sure phenotype doesn't change after baseline visit
  df <- df %>% 
    mutate(visit = factor(visit, levels = c('bl','12m','24m')),
           phenotype = factor(phenotype)) %>%
    group_by(src_subject_id) %>%  
    dplyr::mutate(baseline_pheno = first(phenotype)) %>% 
    dplyr::mutate(phenotype = baseline_pheno) %>% 
    dplyr::select(-baseline_pheno) 
  
  # Remove rows where phenotype is NA
  #but first print warning and say how many folks are getting removed
  phenotype_nas<- df[is.na(df$phenotype),]
  print(paste0('removing ',nrow(phenotype_nas),
               ' subjects because they have NA for phenotype. This generally',
               ' should not happen. Below are the subject IDs and visit dates ',
               'for these people. They should be inspected and fixed in redcap'))
  print(paste0(phenotype_nas$src_subject_id,' ',phenotype_nas$visit ))
  df <- df[!is.na(df$phenotype), ]
  
  # Remove rows where phenotype is 'ineligible' or 'withdrawn'
  df <- df[!(df$phenotype %in% c("ineligible", "withdrawn")), ]
  
  
  # create a site variable based on src_institution_name
  ## not over-writing with rename(), so that redcap_event_name can do a "soft retire"
  df %>% dplyr::mutate(site = src_institution_id) -> df
  # get rid of deprecated variable names is good practice
  df <- subset(df, select = -src_institution_id)
  
  # convert dates
  df$int_diff <- as.numeric(df$int_end - df$int_start)
  df$interview_date <- df$int_start
  
  # remove dob
  df <- subset(df, select = -subject_dob)
  
}