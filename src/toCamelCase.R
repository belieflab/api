toCamelCase <- function(input_string) {
  # Check if the input string contains any uppercase letters in the middle
  # of the word (camelCase).
  if (grepl("[a-z][A-Z]", input_string)) {
    # Split the string by uppercase letters and then capitalize the first
    # letter of each part except the first one.
    parts <- unlist(strsplit(input_string, "[A-Z]"))
    camel_case <- paste0(
      tolower(parts[1]),
      sapply(parts[-1], function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x))))
    )
    return(camel_case)
  } else {
    # The string is already in camelCase.
    return(input_string)
  }
}

snakeToCamel <- function(input_string) {
  # Split the input string by underscores and capitalize the first letter of each part
  parts <- tolower(strsplit(input_string, "_", fixed = TRUE)[[1]])
  camel_case <- paste0(parts[1], sapply(parts[-1], function(x) paste0(toupper(substr(x, 1, 1)), substr(x, 2, nchar(x)))))
  return(camel_case)
}

checkCase <- function(input_string) {
  if (grepl("[a-z][A-Z]", input_string)) {
    # Check if the input string contains any lowercase followed by uppercase letters.
    return("camelCase")
  } else if (grepl("[a-z][A-Z]", sub("^\\w", "", input_string))) {
    # Check if the input string (after removing the first character) contains
    # any lowercase followed by uppercase letters.
    return("PascalCase")
  } else if (grepl("_", input_string)) {
    # Check if the input string contains underscores.
    return(snakeToCamel(input_string))
  } else if (grepl("-", input_string)) {
    # Check if the input string contains hyphens.
    return("kebab-case")
  } else {
    # If none of the above patterns are found, return "unknown".
    return("unknown")
  }
}