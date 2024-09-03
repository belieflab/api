concatenateCsv <- function(directory, version = "") {
  # Get a list of all CSV files in the directory
  csv_files <- list.files(directory, pattern = "\\.csv$", full.names = TRUE)
  
  # Initialize an empty list to hold dataframes
  data_list <- list()
  
  # Loop through each CSV file
  for (file in csv_files) {
    # Read the CSV file
    df <- read.csv(file, stringsAsFactors = FALSE)
    
    # Add the version column
    df$version <- version
    
    # Append the dataframe to the list
    data_list[[length(data_list) + 1]] <- df
  }
  
  # Concatenate all dataframes in the list into one dataframe
  combined_df <- do.call(rbind, data_list)
  
  return(combined_df)
}