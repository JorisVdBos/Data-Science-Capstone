# Filter an input string to an input into a model. This is basically the last two words, with the same filters used to make the string
modelInput <- function(string, mode = "model"){
  
  if(length(string) == 0) return(NULL)
  
  # Pre filtering
  string <- gsub(" $", "", string)
  
  # Make the string a corpus
  Encoding(string) <- "UTF-8"
  string <- Corpus(VectorSource(string))
  
  # Apply corpus filter
  string <- corpusFilter(string)
  
  # Extra filtering
  
  # Split the string to return the last 2 words
  if((class(string[[1]]) == "character")[1])
    string <- strsplit(string[[1]],split=" ") else
      string <- strsplit(string[[1]]$content,split=" ")
  
  if(mode == "testing") return(string[[1]]) else
    return(list(word1 = string[[1]][length(string[[1]])-2],
                word2 = string[[1]][length(string[[1]])-1]))
}

# Prediction function
predict.FreqModel <- function(model, word1 = NULL, word2 = NULL){
  
  doNotPredictIndex <- which(model$wordFreqTable$word %in% doNotPredict)
  
  if("\n" %in% doNotPredict)
    doNotPredictIndex <- c(0, doNotPredictIndex)
  
  # Check default giveNumberOfPossibilities
  if(is.null(giveNumberOfPossibilities)) giveNumberOfPossibilities <- 3
  
  # Post filtering
  word1 <- gsub(" $", "", word1)
  word1 <- modelInput(word1)$word2
  word2 <- modelInput(word2)$word1
  
  # If word1 is empty, regard the input as the beginning of a sentence:
  if(is.null(word1)){
    word1 <- "/n"
  }
  
  # If word2 is empty, check if the input of word1 is a sentence. If so split it up. If not, regard the input as a first word of a sentence:
  if(is.null(word2)){
    word1Split <- strsplit(word1, " ")[[1]]
    if(length(word1Split) == 1){
      word2 <- word1
      word1 <- "/n"
    } else {
      word1ModelInput <- modelInput(word1)
      word1 <- word1ModelInput$word1
      word2 <- word1ModelInput$word2
    }
  }
  
  # Finding input word indices
  if(length(word1) == 0 || word1 == "\n") inputWord1Index <- 0 else
    inputWord1Index <- which(model$wordFreqTable$word == word1)
  
  if(length(word1) > 1) {
    print("length(word1) > 1")
    print(word1)
  }
  
  if(length(word2) == 0 || word2 == "\n") inputWord2Index <- 0 else
    inputWord2Index <- which(model$wordFreqTable$word == word2)
  
  # Find solutions in the 3-grams table
  solutions <- model$n3gramsTable[indexWord1 == inputWord1Index & indexWord2 == inputWord2Index]
  # Take the first three solutions
  solutions <- solutions[-which(solutions$indexWord3 %in% doNotPredictIndex),]
  solutions <- data.table(value = solutions$indexWord3[1:giveNumberOfPossibilities])
  
  solutions$source <- rep("n3gramsTable", giveNumberOfPossibilities)
  
  # Find more solutions in the 2-grams table if necessairy
  findMore <- sum(is.na(solutions$value))
  if(findMore > 0){
    sol2gram <- model$n2gramsTable[indexWord1 == inputWord2Index]$indexWord2
    
    for(i in 1:findMore){
      i <- as.numeric(i)
      sol <- sol2gram[i]
      
      # Check if the found solutions has allready been found in the 3-gram before
      k <- i
      while((sol %in% solutions$value) || (sol %in% doNotPredictIndex)){
        k <- k + 1
        sol <- sol2gram[k]
        if(is.na(sol)) break
      }
      
      # Add 2gram solution to solutions vector
      solutions$value[giveNumberOfPossibilities - findMore + i] <- sol
      solutions$source[giveNumberOfPossibilities - findMore + i] <- "2gramsTable"
    }
  }
  
  # Find more solutions in the freqency table if necessairy
  findMore <- sum(is.na(solutions$value))
  if(findMore > 0){
    
    for(i in 1:findMore){
      # Check if the found solutions has allready been found in the 3-gram before
      k <- as.numeric(i)
      while((k %in% solutions$value) || (k %in% doNotPredictIndex)){
        k <- k + 1
      }
      
      # Add solution to solutions vector
      solutions$value[giveNumberOfPossibilities - findMore + i] <- k
      solutions$source[giveNumberOfPossibilities - findMore + i] <- "wordFreqencyTable"
    }
  }
  
  solutions$value <- as.numeric(solutions$value)
  # Replace indices by words
  getWord <- function(index){
    if(index == 0) return("\n") else
      return(model$wordFreqTable[index, "word", with = FALSE]$word)
  }
  solutions[, value := sapply(solutions$value, function(x) getWord(x))]
  
  # Return solutions vector
  solutions
}