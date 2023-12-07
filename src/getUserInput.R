getUserInput <- function(prompt_text) {
  cat(prompt_text)
  user_input <- readline()
  return(user_input)
}