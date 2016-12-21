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


# Prediction function freqModel
predict.FreqModel <- function(model, string = NULL){
  
  input <- predictionInput(model, string)
  
  inputWord1Index <- input$inputWord1Index
  inputWord2Index  <- input$inputWord2Index
  predictPart <- input$predictPart
  inputWord3Part <- input$inputWord3Part
  inputWord3PartIndices <- input$inputWord3PartIndices
  doNotPredictIndex <- input$doNotPredictIndex
  
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

# Prediction function KNModel
predict.KNModel <- function(model, string = NULL){
  input <- predictionInput(model, string)
  
  inputWord1Index <- input$inputWord1Index
  inputWord2Index  <- input$inputWord2Index
  predictPart <- input$predictPart
  inputWord3Part <- input$inputWord3Part
  inputWord3PartIndices <- input$inputWord3PartIndices
  doNotPredictIndex <- input$doNotPredictIndex
  
  
  # Calculate probabilities from the 2-grams
  solutions2grams <- model$n2gramsTable[indexWord1 == inputWord2Index]
  total3grams <- dim(model$n3gramsTable[indexWord2 == inputWord2Index])[1]
  
  temp <- model$n3gramsTable[indexWord2 == inputWord2Index]
  part3grams <- sapply(solutions2grams$indexWord2,
                       function(x) dim(temp[indexWord3 == x])[1])
  
  if(length(part3grams) != 0)
    solutions2grams$prob <- part3grams/total3grams else
      solutions2grams[, prob := freq]
  
  
  # Calculate probabilities from the 3-grams
  kneserNeyAbsDiscount <- dim(model$wordFreqTable)[1]/(dim(model$wordFreqTable)[1] +
                                                         2*dim(model$n2gramsTable)[1])
  solutions3grams <- model$n3gramsTable[indexWord2 == inputWord2Index & 
                                          indexWord1 == inputWord1Index]
  solutions3grams[, term1 := max(freq - kneserNeyAbsDiscount,
                                 0), by = "indexWord3"]
  total3gramsInput <- sum(solutions3grams$freq)
  solutions3grams$term1 <- solutions3grams$term1 / total3gramsInput
  
  solutions3grams[, term2 := kneserNeyAbsDiscount*
                    sum(model$n3gramsTable[indexWord2 == inputWord2Index &
                                             indexWord1 == inputWord1Index]$freq)/
                    total3gramsInput]
  solutions3grams[, term2 := term2*solutions2grams[solutions2grams$indexWord2 %in% indexWord3]$prob, by = "indexWord3"]
  
  solutions3grams[, prob := term1 + term2, by = "indexWord3"]
  
  # Get solutions from 1-grams
  solutionsFreq <- model$wordFreqTable[order(-freq)][1:(3*giveNumberOfPossibilities)]
  
  # Compare all probabilities and choose the top scoring
  solutions2grams <- solutions2grams[, c("indexWord2", "prob"), with = FALSE]
  names(solutions2grams) <- c("sol", "prob")
  solutions3grams <- solutions3grams[, c("indexWord3", "prob"), with = FALSE]
  names(solutions3grams) <- c("sol", "prob")
  solutionsFreq <- solutionsFreq[, c("index", "prob"), with = FALSE]
  names(solutionsFreq) <- c("sol", "prob")
  
  solution <- solutions3grams
  solution <- rbind(solution,
                    solutions2grams[!(sol %in% solution$sol)])
  solution <- rbind(solution, 
                    solutionsFreq[!(sol %in% solution$sol)])
  solution <- solution[!(sol %in% doNotPredictIndex)]
  # If a partial solution was given, filter
  if(predictPart) {
    solution <- solution[sol %in% inputWord3PartIndices]
  }
  solution <- solution[prob > 0]
  solution <- solution[order(-prob)][1:giveNumberOfPossibilities]
  names(solution) <- c("value", "prob")
  
  # Construct solution data frame
  getWord <- function(index){
    if(is.na(index))
      return(NA)
    if(index == 0) return("\n") else
      return(model$wordFreqTable[index, "word", with = FALSE]$word)
  }
  solution[, value := sapply(solution$value, function(x) getWord(x))]
  
  return(solution)
}