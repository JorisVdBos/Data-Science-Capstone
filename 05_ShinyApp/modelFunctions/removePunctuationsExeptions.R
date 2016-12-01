
# The corpus filter is defined here, so it can be used later to craft the input into the model
removePunctuationsExeptions <- function(x) {
  # Replace/"save" punctuations to be disgarded, in numbers
  x <- gsub("[0-9]+", "#", x)
  x <- gsub("[#*\\.*|#*,*]*#", "000", x) # A number in any notation
  x <- gsub(",+", " 001 ", x) # Comma
  x <- gsub("\\.+", " 002 ", x) # Point
  x <- gsub("'+", "003", x) # Accent -> Not seperated by spaces to conserve words such as "don't" etc.
  x <- gsub("\\?+", " 004 ", x) # QuestionMark
  x <- gsub("!+", " 005 ", x) # ExclamationMark
  
  # Remove all punctuations
  x <- gsub("[[:punct:]]+", "", x)
  
  # Re-install the saved punctuations
  x <- gsub("( *000)+ *", " # ", x) 
  x <- gsub(" *001 *", " , ", x)
  x <- gsub(" *002 *", " \n ", x) # Dot is replaced by a New line
  x <- gsub(" *003 *", "'", x)
  x <- gsub(" *004 *", " ? ", x)
  x <- gsub(" *005 *", " ! ", x)
  
  # Finally, convert to to lower case
  tolower(x)
}