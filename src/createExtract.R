#' Create Data Extracts in Various Formats
#'
#' This function creates data extracts from a given dataframe in one or more of 
#' the following formats: CSV, RDS, and SPSS.
#' It sources the necessary functions for creating each file type from their 
#' respective scripts and executes them based on the arguments passed. The user 
#' specifies the desired output formats using boolean flags for each file type.
#'
#' @param df The dataframe to be exported.
#' @param df_name The base name for the output file(s) without extension.
#' @param csv Logical; if TRUE, a CSV file is created using the `createCsv` function.
#' @param rds Logical; if TRUE, an RDS file is created using the `createRds` function.
#' @param spss Logical; if TRUE, an SPSS file is created using the `createSpss` function.
#' @importFrom base source
#' @examples
#' createExtract(mtcars, "mtcars_export", csv = TRUE, rds = TRUE, spss = FALSE)
#' @export
#' @author Joshua Kenney <joshua.kenney@yale.edu>

createExtract <- function(df, df_name, csv=NULL, rds=NULL, spss=NULL) {
  
  base::source("api/src/createCsv.R")
  base::source("api/src/createRds.R")
  base::source("api/src/createSpss.R")
  
  if (!is.null(csv) && csv) {
    createCsv(df, df_name)
  }
  if (!is.null(rds) && rds) {
    createRds(df, df_name)
  }
  if (!is.null(spss) && spss) {
    createSpss(df, df_name)
  }
}
