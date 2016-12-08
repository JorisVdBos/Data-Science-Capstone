corpusFilter <- function(corpus){
  ## Punctuations: Dots will be the next line. Comma's can be interperted as a word, see above
  corpus <- tm_map(corpus, content_transformer(removePunctuationsExeptions))
  
  # Some special character cases
  s <- "\u0091"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "'")
  s <- "\u0092"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "'")
  s <- "\u0093"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "")
  s <- "\u0085"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = " \n ")
  s <- "\u0094"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "")
  s <- "\u0096"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "")
  s <- "\u0097"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "")
  
  ## Removing double \n and \n at the beginning and end and putting it between spaces
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "$", replacement = " \n")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "^", replacement = "\n ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = " *(\n *)+", replacement = " \n ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = " +", replacement = " ")
  
  
  corpus
}