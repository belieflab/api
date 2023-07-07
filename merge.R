


multiMerge <- function(..., by = NULL, all = FALSE) {
  data_list <- list(...)
  
  if (length(data_list) < 2) {
    stop("At least two data frames are required for merging.")
  }
  
  invalid_names <- vapply(data_list, function(df) !grepl("_clean", deparse(substitute(df))), logical(1))
  if (any(invalid_names)) {
    invalid_df <- names(data_list)[invalid_names]
    stop(paste("Data frame", invalid_df, "does not contain '_clean' in its name."))
  }
  
  merged_df <- data_list[[1]]
  
  for (i in 2:length(data_list)) {
    if (!grepl("_clean", deparse(substitute(data_list[[i]])))) {
      stop(paste("Data frame", names(data_list)[i], "does not contain '_clean' in its name."))
    }
    merged_df <- base::merge(merged_df, data_list[[i]], by = by, all = all)
  }
  
  return(merged_df)
}

df1_clean <- data.frame(ID = c(1, 2, 3), Value1 = c(10, 20, 30))
df2_clean <- data.frame(ID = c(1, 2, 4), Value2 = c(100, 200, 400))
df3_clean <- data.frame(ID = c(1, 3, 5), Value3 = c(1000, 3000, 5000))
df4 <- data.frame(ID = c(1, 2, 3), Value4 = c(10000, 20000, 30000))

merged_df <- multiMerge(df4, df2_clean, df3_clean,  by = "ID", all = TRUE)
View(merged_df)