loadPackages <- function(libraries) {
  # this function receives a character vector with libraries names and download 
  # them if necessary
  if (class(libraries)!="character") {warning("vector is not character")}
  # loop libraries vector
  for (i in 1:length(libraries)) {
    # Check if the package is already installed
    if (!requireNamespace(libraries[i], quietly = TRUE)) {
      # If not installed, install the package
      install.packages(libraries[i], dependencies = TRUE)
    } else {
      cat(paste("Package", libraries[i], "is already installed.\n"))
    }
  }
  # if (!require(ggplot2)) {install.packages("ggplot2")}; library(ggplot2)
  # if (!require(ggpubr)) {install.packages("ggpubr")}; library(ggpubr)
  # if (!require(reshape2)) {install.packages("reshape2")}; library(reshape2)
  # if (!require(lmerTest)) {install.packages("lmerTest")}; library(lmerTest)
  # if (!require(ggsignif)) {install.packages("ggsignif")}; library(ggsignif)
}