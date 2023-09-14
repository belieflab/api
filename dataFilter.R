dataFilter <- function(df, visit = "bl", end_date = Sys.Date()) {
  df <- df[df$visit == visit, ]
  # df <- subset(df, visit == visit)

  if (df$interview_date) {
    df <- df[df$interview_date <= end_date, ]
  }

  if (df$int_diff) {
    df <- df[df$int_end <= end_date, ]
  }

  return(df)
}
