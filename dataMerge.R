#' Data Merge
#'
#' The code below is meant to simplify data merging across measures.
#' It uses a candidate key of NDA Required Variables to ensure a seamless merge
#' of clean data frames.
#' 
#' @param ... Variable, many clean data frames
#' @param by Optional; nda_merge_vars
#' @param all Optional; OUTER JOIN
#' @param no.dups Optional; keep duplicates
#' 
#' @examples
#' dataMerge("prl", "rgpts", all=FALSE) # INNER JOIN
#' dataMerge("kamin,"lshsr,"sips_p, by = "subjectkey")
#' @return the merged dataframes
#' @author Joshua Kenney <joshua.kenney@yale.edu>

# nda variables required for merging
nda_merge_vars <- c("src_subject_id", "subjectkey", "phenotype", "visit", "sex", "site")

dataMerge <- function(..., by = c("src_subject_id", "subjectkey", "phenotype", "visit", "sex", "site"), all = TRUE, no.dups = FALSE, csv = FALSE, rds = FALSE, spss = FALSE) {
  
  # inner join = FALSE
  # outer join = TRUE

  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)

  data_list <- list(...)

  # Use the provided 'by' and 'all' parameters
  dfs <- Reduce(function(x, y) base::merge(x, y, by = by, all = all, no.dups = no.dups), data_list)
  
  if (csv) {
    createCsv(dfs, "merged_dfs")
  }
  if (rds) {
    createRds(dfs, "merged_dfs")
  }
  if (spss) {
    createSpss(dfs, "merged_dfs")
  }

  return(dfs)
}


# checkMergeDuplicates <- function(df) {
#   if (!require(dplyr)) {
#     install.packages("dplyr")
#   }
#   library(dplyr)
# 
#   if ("visit" %in% colnames(df)) {
#     df$duplicates <- duplicated(df[c("src_subject_id")])
# 
#     df$measure <- duplicated(df)
# 
#     # separate the duplicates to their own df
#     df_dup_ids <- subset(df, duplicates == TRUE)[c("src_subject_id")]
# 
#     # filter only the subject ids that are duplicated to include both iterations
#     df_duplicates <- df %>%
#       filter(src_subject_id %in% df_dup_ids$src_subject_id) %>%
#       select(nda_required_variables, measure.x, measure.y, measure)
# 
#     if (nrow(df_duplicates) == 0) {
#       cat("no duplicates")
#     }
#     if (nrow(df_duplicates) > 0) {
#       View(df_duplicates)
#       cat("Duplicates detected.\nPlease contact your data admin to remove duplicates from Qualtrics.")
#       return(df_duplicates)
#     }
#   }
# }
