#' Data Filter
#'
#' Filters a dataframe based on specified 'visit', 'week', and 'state' parameters and 
#' selects columns based on 'columns_of_interest'. If 'columns_of_interest' is not provided,
#' retains all columns by default. Filters by 'visit' and 'week' if these columns exist
#' and values for them are provided.
#'
#' @param df Dataframe to be filtered and trimmed based on the provided parameters.
#' @param rows_of_interest Optional; a vector of row names to be retained in the final output. 
#'        If NULL or empty, all rows in the dataframe are retained.
#' @param columns_of_interest Optional; a vector of column names to be retained in the final output. 
#'        If NULL or empty, all columns in the dataframe are retained.
#' @param visit Optional; a specific visit value to filter the dataframe by. Only used if 
#'        'visit' column exists in the dataframe.
#' @param week Optional; a specific week value to filter the dataframe by. Only used if 
#'        'week' column exists in the dataframe.
#' @param states Optional; a vector of state conditions to filter the dataframe by. Only used 
#'        if 'state' column exists in the dataframe. Can include values like 'complete', 
#'        'completed baseline', 'completed 12m', 'completed 24m', etc.
#'
#' @return A filtered dataframe based on the provided 'visit', 'week', and 'state' parameters, 
#'         and containing only the columns specified in 'columns_of_interest'. If no columns 
#'         are specified, returns the entire dataframe with applied row filters.
#'
#' @examples
#' data_filtered <- dataFilter(df, 
#'                             rows_of_interest = c("foo","bar"),
#'                             columns_of_interest = c("src_subject_id", "phenotype"), 
#'                             visit = 2, 
#'                             states = c("complete", "completed baseline"))
#' @import dplyr
#' @export

dataFilter <- function(df, rows_of_interest = NULL, columns_of_interest = NULL,
                       visit = NULL, week = NULL, states = NULL, arm = NULL, site=NULL) {
  
  if (!require(dplyr, quietly = TRUE)) {install.packages("dplyr")}; library(dplyr)
  
  # Define timepoints and candidate keys
  timepoints <- c("visit", "week")
  config <- config::get()
  
  if (config$study_alias == "capr") {
    # NDA variables suitable for merging fromr capr
    super_key <- c("src_subject_id", "subjectkey", "phenotype", "sex", "site", "arm")
  } else {
    super_key <- c("src_subject_id", "subjectkey", "phenotype", "sex", "site", "arm", "state", "PROLIFIC_PID", "participantId", "workerId", "rat_id")
  }
  
  # Detect existing keys and timepoints in the dataframe
  detected_timepoints <- intersect(timepoints, names(df))
  detected_keys <- intersect(super_key, names(df))
  
  # Ensure 'columns_of_interest' is a non-null vector
  if (!is.null(columns_of_interest) && length(columns_of_interest) > 0) {
    columns_of_interest <- unique(c(detected_keys, detected_timepoints, columns_of_interest))
  } else {
    # If no specific columns of interest are provided, use all columns
    columns_of_interest <- names(df)
    message("No columns of interest provided; all columns will be included.")
  }
  
  # Ensure 'rows_of_interest' is a non-null vector
  if (is.null(rows_of_interest)) {
    rows_of_interest <- rep(T,nrow(df))
  }
  
  # Filter by 'visit' or 'week' if applicable
  if ("visit" %in% detected_timepoints && !is.null(visit)) {
    message("Filtering by visit: ", visit)
    df <- df %>% dplyr::filter(visit == !!visit)
  } else if ("week" %in% detected_timepoints && !is.null(week)) {
    message("Filtering by week: ", week)
    df <- df %>% dplyr::filter(week == !!week)
  }
  
  # Filter by 'state' if applicable and if column exists
  if ("state" %in% names(df) && !is.null(states)) {
    message("Filtering by state: ", toString(states))
    df <- df %>% dplyr::filter(state %in% !!states)
  }
  
  # Filter by 'state' if applicable and if column exists
  if ("arm" %in% names(df) && !is.null(arm)) {
    message("Filtering by arm: ", toString(arm))
    df <- df %>% dplyr::filter(arm %in% !!arm)
  }
  
  # Filter by 'state' if applicable and if column exists
  if ("site" %in% names(df) && !is.null(site)) {
    message("Filtering by site: ", toString(site))
    df <- df %>% dplyr::filter(site %in% !!site)
  }
  
  # Filtering based on columns of interest (including any existing keys and timepoints)
  # message("Selecting columns of interest: ", toString(columns_of_interest))
  # df <- df[rows_of_interest, names(df) %in% columns_of_interest]
  
  return(df)
}
