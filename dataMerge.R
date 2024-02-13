# Get full file paths of all R files in the api directory
# base::source all files using lapply()

# nda variables required for merging
nda_merging_variables <- c("src_subject_id", "subjectkey", "phenotype", "visit", "sex", "site")

dataMerge <- function(..., by = c(nda_merging_variables), all = TRUE, no.dups = FALSE) {
  # inner join = FALSE
  # outer join = TRUE

  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)

  data_list <- list(...)

  # Use the provided 'by' and 'all' parameters
  dfs <- Reduce(function(x, y) base::merge(x, y, by = by, all = all, no.dups = no.dups), data_list)

  # Clean Up
  # suppressWarnings(source("api/env/cleanup.R"))

  return(dfs)
}


checkMergeDuplicates <- function(df) {
  if (!require(dplyr)) {
    install.packages("dplyr")
  }
  library(dplyr)

  if ("visit" %in% colnames(df)) {
    df$duplicates <- duplicated(df[c("src_subject_id")])

    df$measure <- duplicated(df)

    # separate the duplicates to their own df
    df_dup_ids <- subset(df, duplicates == TRUE)[c("src_subject_id")]

    # filter only the subject ids that are duplicated to include both iterations
    df_duplicates <- df %>%
      filter(src_subject_id %in% df_dup_ids$src_subject_id) %>%
      select(nda_required_variables, measure.x, measure.y, measure)

    if (nrow(df_duplicates) == 0) {
      cat("no duplicates")
    }
    if (nrow(df_duplicates) > 0) {
      View(df_duplicates)
      cat("Duplicates detected.\nPlease contact your data admin to remove duplicates from Qualtrics.")
      return(df_duplicates)
    }
  }
}
