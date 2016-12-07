# Filter an input string to an input into a model. This is basically the last two words, with the same filters used to make the string
modelInput <- function(string, mode = "model"){
  # Check input
  if(length(string) == 0 || string == "") 
    return(list(word1 = "\n",
                word2 = "\n"))
  
  
  # Pre filtering
  string <- gsub(" $", "", string)
  
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
  
  if(mode == "testing") return(string[[1]]) else
    return(list(word1 = string[[1]][length(string[[1]])-2],
                word2 = string[[1]][length(string[[1]])-1]))
}


# Prediction function
predict.FreqModel <- function(model, string = NULL){
  
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
  
  # Find solutions in the 3-grams table
  solutions <- model$n3gramsTable[indexWord1 == inputWord1Index & indexWord2 == inputWord2Index]
  
  # Filter solutions
  if(predictPart)
    solutions <- solutions[indexWord3 %in% inputWord3PartIndices]
  solutions <- solutions[!(indexWord3 %in% doNotPredictIndex)]
  
  # Take the first three solutions
  solutions <- data.table(value = solutions$indexWord3[1:giveNumberOfPossibilities])
  
  solutions$source <- rep("n3gramsTable", giveNumberOfPossibilities)
  
  # Find more solutions in the 2-grams table if necessairy
  findMore <- sum(is.na(solutions$value))
  if(findMore > 0){
    sol2gram <- model$n2gramsTable[indexWord1 == inputWord2Index]$indexWord2
    
    # Filter partial
    if(predictPart)
      sol2gram <- intersect(sol2gram, inputWord3PartIndices)
    
    k <- 1
    for(i in 1:findMore){
      sol <- sol2gram[k]
      
      # Check if the found solutions has allready been found in the 3-gram before
      while((sol %in% solutions$value) || 
            (sol %in% doNotPredictIndex)){
        k <- k + 1
        sol <- sol2gram[k]
        if(is.na(sol)) break
      }
      
      # Add 2gram solution to solutions vector
      solutions$value[giveNumberOfPossibilities - findMore + i] <- sol
      solutions$source[giveNumberOfPossibilities - findMore + i] <- "2gramsTable"
      k <- k + 1
    }
  }
  
  # Find more solutions in the freqency table if necessairy
  findMore <- sum(is.na(solutions$value))
  if(findMore > 0){
    if(predictPart)
      solFreq <- inputWord3PartIndices else
        solFreq <- 1:length(model$wordFreqTable$word)
      
    k <- 1
    for(i in 1:findMore){
      sol <- solFreq[k]
      
      # Check if the found solutions has allready been found in the 3-gram before
      while((sol %in% solutions$value) || 
            (sol %in% doNotPredictIndex)){
        k <- k + 1
        sol <- solFreq[k]
        if(is.na(sol)) break
      }
      
      # Add solution to solutions vector
      solutions$value[giveNumberOfPossibilities - findMore + i] <- sol
      solutions$source[giveNumberOfPossibilities - findMore + i] <- "wordFreqencyTable"
      k <- k + 1
    }
  }
  
  solutions$value <- as.numeric(solutions$value)
  # Replace indices by words
  getWord <- function(index){
    if(is.na(index))
      return(NA)
    if(index == 0) return("\n") else
      return(model$wordFreqTable[index, "word", with = FALSE]$word)
  }
  solutions[, value := sapply(solutions$value, function(x) getWord(x))]
  
  # Return solutions vector
  solutions
}