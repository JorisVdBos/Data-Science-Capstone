# Filter an input string to an input into a model. This is basically the last two words, with the same filters used to make the string
modelInput <- function(string, mode = "model"){
  
  if(length(string) == 0) return(NULL)
  
  # Make the string a corpus
  Encoding(string) <- "UTF-8"
  string <- Corpus(VectorSource(string))
  
  # Apply corpus filter
  string <- corpusFilter(string)
  
  # Extra filtering
  
  # Split the string to return the last 2 words
  string <- strsplit(string[[1]],split=" ")
  
  if(mode == "testing") return(string[[1]]) else
    return(list(word1 = string[[1]][length(string[[1]])-1],
                word2 = string[[1]][length(string[[1]])]))
}