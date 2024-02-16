#' Data Filter
#'
#' The code below is meant to simplify data filtering.
#' It uses a candidate key of NDA Required Variables to quickly filter data and
#' prepare them to be used by Data Merge.
#' 
#' @param ... Variable, many clean data frames
#' @param by Optional; nda_merge_vars
#' @param all Optional; OUTER JOIN
#' @param no.dups Optional; keep duplicates
#' 
#' @examples
#' dataMerge("prl", "rgpts", all=FALSE) # INNER JOIN
#' dataMerge("kamin,"lshsr,"sips_p, by = "subjectkey")
#' @return the filtered dataframe
#' @author Joshua Kenney <joshua.kenney@yale.edu>


dataFilter <- function(df, columns_of_interest = NULL, visit = "bl") {
  
  nda_merge_vars <- c("src_subject_id", "subjectkey", "phenotype", "visit", "sex", "site")
  
  # filtering based on columns of interest
  df <- df[, names(df) %in% c(nda_merge_vars, columns_of_interest)]
  
  # filtering based on time point
  # df <- subset(df, visit == visit)
  
  return(df)
}

# dataFilter <- function(df, visit = "bl", end_date = Sys.Date()) {
#   df <- df[df$visit == visit, ]
#   # df <- subset(df, visit == visit)
# 
#   if (df$interview_date) {
#     df <- df[df$interview_date <= end_date, ]
#   }
# 
#   if (df$int_diff) {
#     df <- df[df$int_end <= end_date, ]
#   }
# 
#   return(df)
# }




# columns_of_interest <- c("eod_total")
# eod_clean <- eod_clean[, names(eod_clean) %in% c(nda_merge_vars, columns_of_interest)]
# columns_of_interest <- c("demo_race", "demo_ethnicity", "demo_gender", "sex", "demo_age_in_years") # some ages look weird 
# demo_clean <- demo_clean[, names(demo_clean) %in% c(nda_merge_vars, columns_of_interest)]
# columns_of_interest <- c("kamin_blocking_score", "kamin_control_score")
# kamin_clean <- kamin_clean[, names(kamin_clean) %in% c(nda_merge_vars, columns_of_interest)]
# columns_of_interest <- c("pdi21_p_conviction", "pdi21_d_conviction")
# pdi21_clean <- pdi21_clean[, names(pdi21_clean)  %in% c(nda_merge_vars, columns_of_interest)]
# columns_of_interest <- c("rgpts_total", "rgpts_persecution", "rgpts_reference")
# rgpts_clean <- rgpts_clean[, names(rgpts_clean)  %in% c(nda_merge_vars, columns_of_interest)]