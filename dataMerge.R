#' Data Merge
#'
#' This function simplifies the process of merging multiple cleaned data frames by automatically determining 
#' common merge keys or utilizing user-specified keys. Supports both inner and outer join methods, 
#' and offers options for exporting the merged data.
#'
#' @param ... Clean data frames to be merged.
#' @param by A vector of strings specifying the column names to be used as merge keys. If NULL, 
#'           the function automatically determines common keys from the provided data frames.
#' @param all Logical; if TRUE, performs an OUTER JOIN. If FALSE, performs an INNER JOIN.
#' @param no.dups Logical; if TRUE, duplicates are removed post-merge.
#' @param csv Logical; if TRUE, the merged data frame is exported as a CSV file.
#' @param rds Logical; if TRUE, the merged data frame is saved as an RDS file.
#' @param spss Logical; if TRUE, the merged data frame is exported as an SPSS file.
#'
#' @examples
#' # Perform an OUTER JOIN on 'prl' and 'rgpts' using default keys:
#' dataMerge(prl, rgpts, all = TRUE)
#' # Perform an INNER JOIN using 'subjectkey' as the merge key:
#' dataMerge(prl, rgpts, by = "subjectkey", all = FALSE)
#' # Merge multiple data frames using specified keys and export to CSV:
#' dataMerge(prl, rgpts, lshsr, by = c("src_subject_id", "visit"), csv = TRUE)
#'
#' @return A merged data frame based on the specified or common candidate keys.
#' @author Joshua Kenney <joshua.kenney@yale.edu>
#' @export
dataMerge <- function(..., by = NULL, all = TRUE, no.dups = FALSE, csv = FALSE, rds = FALSE, spss = FALSE) {
  
  if(!require(dplyr, quietly = TRUE)) {install.packages("dplyr")}; library(dplyr)
  
  # Inform about the type of join being performed
  message(ifelse(all, "Performing an OUTER JOIN:", "Performing an INNER JOIN:"))
  
  merging_variables <- c("src_subject_id", "subjectkey", "phenotype", "visit", "week", "sex", "site", "arm", "state")
  
  # Load custom scripts if any
  lapply(list.files("api/src", pattern = "\\.R$", full.names = TRUE), base::source)
  lapply(list.files("api/fn", pattern = "\\.R$", full.names = TRUE), base::source)
  
  data_list <- list(...)
  
  # Determine the keys to use for merging
  if (is.null(by)) {
    common_candidate_keys <- Reduce(intersect, lapply(data_list, function(df) intersect(merging_variables, names(df))))
    message("Detected common candidate keys for merge: ", toString(common_candidate_keys))
  } else {
    common_candidate_keys <- by
    message("Using user-specified keys for merge: ", toString(common_candidate_keys))
  }
  
  # Preprocess data frames
  data_list <- lapply(data_list, function(df) df %>% select(-any_of(c("interview_date", "interview_age"))))
  
  # Perform the merging process
  dfs <- Reduce(function(x, y) base::merge(x, y, by = common_candidate_keys, all = all, no.dups = no.dups), data_list)
  
  # Export merged data if requested
  if (csv) { createCsv(dfs, "merged_dfs.csv") }
  if (rds) { createRDS(dfs, "merged_dfs.rds") }
  if (spss) { createSpss(dfs, "merged_dfs.sav") }
  
  
  return(dfs)
}
