

multiMerge <- function(..., by = NULL, all = FALSE) {
  data_list <- list(...)
  
  if (length(data_list) < 2) {
    stop("At least two data frames are required for merging.")
  }
  
  merged_df <- data_list[[1]]
  
  for (i in 2:length(data_list)) {
    merged_df <- base::merge(merged_df, data_list[[i]], by = by, all = all)
  }
  
  return(merged_df)
}

df1 <- data.frame(ID = c(1, 2, 3), Value1 = c(10, 20, 30))
df2 <- data.frame(ID = c(1, 2, 4), Value2 = c(100, 200, 400))
df3 <- data.frame(ID = c(1, 3, 5), Value3 = c(1000, 3000, 5000))

merged_df <- multiMerge(df1, df2, df3, by = "ID", all = TRUE)
View(merged_df)