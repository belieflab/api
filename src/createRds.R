#' Create RDS File from a DataFrame
#'
#' This function exports a given R DataFrame to an RDS file format.
#' The resulting file is saved in the "./tmp/" directory. If a filename
#' is not specified, the function uses the name of the DataFrame variable.
#' The ".Rda" extension is appended automatically to the filename.
#'
#' @param df DataFrame to be exported to RDS format.
#' @param df_name Optional; a custom file name for the saved RDS file.
#'   If not provided, the name of the DataFrame variable is used.
#'   The function adds the ".Rda" extension automatically.
#' @return The function writes an RDS file to the specified path and prints a message
#'   indicating the file's location. This function does not return a value.
#' @examples
#' createRds(kamin)
#' @export
#' @author Joshua Kenney <joshua.kenney@yale.edu>

createRds <- function(df, df_name = NULL) {
  # Use df_name if provided, otherwise derive from df variable name
  filename <- if (!is.null(df_name)) {
    df_name
  } else {
    deparse(substitute(df))
  }
  
  # Construct the file path
  path <- paste0("./tmp/", filename, '.Rda')
  
  # Write the DataFrame to an RDS file
  saveRDS(df, file = path)
  
  # Notify user of file creation
  message(paste0("Extract created at ", path, "\n"))
}
