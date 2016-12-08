# Filter an input string to an input into a model. This is basically the last two words, with the same filters used to make the string
modelInput <- function(string, mode = "model"){
  # Check input
  if(length(string) == 0 || 
     string == "")
    return(list(word1 = "\n",
                word2 = "\n"))
  
  
  # Pre filtering
  string <- gsub(" $", "", string)
  string <- gsub("\n", ".", string)
  
  # Make the string a corpus
  Encoding(string) <- "UTF-8"
  string <- Corpus(VectorSource(string))
  
  # Apply corpus filter. This function can be found in the map "load" and 
  # should be the same filter used to construct the model
  string <- corpusFilter(string)
  
  # Split the string to return the last 2 words
  if((class(string[[1]]) == "character")[1])
    string <- strsplit(string[[1]],split=" ") else
      string <- strsplit(string[[1]]$content,split=" ")
  
  word1 <- string[[1]][length(string[[1]])-2]
  word2 <- string[[1]][length(string[[1]])-1]
  if(length(word1) == 0 || 
     word1 == "")
    word1 <- "\n"
  if(length(word2) == 0 || 
     word2 == "")
    word2 <- "\n"
  
  if(mode == "testing") return(string[[1]][3:length(string[[1]])]) else
    return(list(word1 = word1,
                word2 = word2))
}

# Predicted input string filter
predictionInput <- function(model, string){
  
  # Check if the input ends with a space. If not, we can try to match
  # the partial word
  if(length(grep(" $", string)) == 0 && string != ""){
    predictPart <- TRUE
    
    # Get third 3 partial and indices
    inputWord3Part <- strsplit(string, split = " ")[[1]]
    inputWord3Part <- inputWord3Part[length(inputWord3Part)]
    inputWord3Part <- modelInput(inputWord3Part)$word2
    inputWord3PartIndices <- grep(paste0("^", inputWord3Part), model$wordFreqTable$word)
    
    # Remove word from input string
    string <- gsub(paste0(" ", inputWord3Part, "$"), "", string)
  } else {
    inputWord3Part <- character()
    inputWord3PartIndices <- integer()
    predictPart <- FALSE
  }
  
  # Find which indices are off limits
  doNotPredictIndex <- which(model$wordFreqTable$word %in% doNotPredict)
  if("\n" %in% doNotPredict)
    doNotPredictIndex <- c(0, doNotPredictIndex)
  
  # Check default giveNumberOfPossibilities
  if(is.null(giveNumberOfPossibilities)) giveNumberOfPossibilities <- 3
  
  # Post filtering
  word1 <- modelInput(string)$word1
  word2 <- modelInput(string)$word2
  
  # Finding input word indices
  if(word1 == "\n") inputWord1Index <- 0 else
    inputWord1Index <- which(model$wordFreqTable$word == word1)
  if(length(word2) == 0 || word2 == "\n") inputWord2Index <- 0 else
    inputWord2Index <- which(model$wordFreqTable$word == word2)
  
  return(list(inputWord1Index = inputWord1Index,
              inputWord2Index = inputWord2Index,
              predictPart = predictPart,
              inputWord3Part = inputWord3Part,
              inputWord3PartIndices = inputWord3PartIndices,
              doNotPredictIndex = doNotPredictIndex))
}