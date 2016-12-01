source("01_Load/load.R")
source("02_ExploratoryAnalysis/Explore.R")
source("03_Modelling/General.R")

# This first model looks at the freqency table of the 3-grams and takes the three options that are most frequent. If less than three options are found, it will look at the 2-grams table
predict.FreqModel <- function(model, word1 = NULL, word2 = NULL){
  
  doNotPredictIndex <- which(model$wordFreqTable$word %in% doNotPredict)
  
  if("\n" %in% doNotPredict)
    doNotPredictIndex <- c(0, doNotPredictIndex)
    
  # Check default giveNumberOfPossibilities
  if(is.null(giveNumberOfPossibilities)) giveNumberOfPossibilities <- 3
  
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

# Create a model object
createFreqModel <- function(corpus, tdm, freqencyCutoff = 1, loadingBar = TRUE) {
  loadingSteps <- 10
  if(loadingBar)
    pb <- txtProgressBar(style = 3)
  
  # Create tables
  wordFreqTable <- wordFreq(tdm)
  wordFreqTable <- wordFreqTable[order(-freq)]
      if(loadingBar)
       setTxtProgressBar(pb, 1/loadingSteps)
  n2grams <- ngramsFromCorpus(corpus, n = 2)
  n2gramsTable <- data.table(get.phrasetable(n2grams))
  n2gramsTable <- n2gramsTable[freq > freqencyCutoff]
  n2gramsTable[, ngrams := gsub(" $", "", ngrams)]
      if(loadingBar)
       setTxtProgressBar(pb, 2/loadingSteps)
  n3grams <- ngramsFromCorpus(corpus, n = 3)
  n3gramsTable <- data.table(get.phrasetable(n3grams))
  n3gramsTable <- n3gramsTable[freq > freqencyCutoff]
  n3gramsTable[, ngrams := gsub(" $", "", ngrams)]
      if(loadingBar)
       setTxtProgressBar(pb, 3/loadingSteps)
  
  # Converting the words to numbers
  wordFreqTable[, index := 1:length(wordFreqTable$word)]
  wordFreqTable <- rbindlist(list(wordFreqTable, data.frame("\n", 0, 0)))
  wordFreqTable$word <- as.character(wordFreqTable$word)
  setkey(wordFreqTable, "word")
      if(loadingBar)
       setTxtProgressBar(pb, 4/loadingSteps)
  
  n2gramsTable[, word1 := gsub(" .+", "", ngrams)]
  setkey(n2gramsTable, "word1")
  n2gramsTable <- n2gramsTable[wordFreqTable]
  setnames(n2gramsTable, "index", "indexWord1")
  
  if(loadingBar)
   setTxtProgressBar(pb, 5/loadingSteps)
  
  n2gramsTable[, word2 := gsub(".+ ", "", ngrams)]
  setkey(n2gramsTable, "word2")
  n2gramsTable <- n2gramsTable[wordFreqTable]
  setnames(n2gramsTable, "index", "indexWord2")
  
  if(loadingBar)
   setTxtProgressBar(pb, 6/loadingSteps)
  
  n3gramsTable[, word1 := gsub(" .+", "", ngrams)]
  setkey(n3gramsTable, "word1")
  n3gramsTable <- n3gramsTable[wordFreqTable]
  setnames(n3gramsTable, "index", "indexWord1")
  
  if(loadingBar)
   setTxtProgressBar(pb, 7/loadingSteps)
  
  n3gramsTable[, word2 := sub("(['|\n|,|0-9A-Za-z]+)", "", ngrams)]
  n3gramsTable[, word2 := sub("^ ", "", word2)]
  n3gramsTable[, word2 := sub(" .+", "", word2)]
  setkey(n3gramsTable, "word2")
  n3gramsTable <- n3gramsTable[wordFreqTable]
  setnames(n3gramsTable, "index", "indexWord2")
  
  if(loadingBar)
   setTxtProgressBar(pb, 8/loadingSteps)
  
  n3gramsTable[, word3 := gsub("^.+ ", "", ngrams)]
  setkey(n3gramsTable, "word3")
  n3gramsTable <- n3gramsTable[wordFreqTable]
  setnames(n3gramsTable, "index", "indexWord3")
  
  if(loadingBar)
   setTxtProgressBar(pb, 9/loadingSteps)
  
  # Reduce size by dropping all columns except the Index
  wordFreqTable <- wordFreqTable[order(-freq)]
  wordFreqTable <- wordFreqTable[,"word", with = FALSE]
  n2gramsTable <- n2gramsTable[!is.na(n2gramsTable$freq)]
  n2gramsTable <- n2gramsTable[order(-freq)]
  n2gramsTable <- n2gramsTable[,c("indexWord1", "indexWord2"), with = FALSE]
  n3gramsTable <- n3gramsTable[!is.na(n3gramsTable$freq)]
  n3gramsTable <- n3gramsTable[order(-freq)]
  n3gramsTable <- n3gramsTable[,c("indexWord1", "indexWord2", "indexWord3"), with = FALSE]
  
  if(loadingBar)
   setTxtProgressBar(pb, 10/loadingSteps)
  
  # Output
  output <- list(wordFreqTable = wordFreqTable, 
                 n2gramsTable = n2gramsTable, 
                 n3gramsTable = n3gramsTable)
  class(output) <- append(class(output),"FreqModel")
  
  if(loadingBar)
    close(pb)
  
  output
}

createFreqModel2 <-  function(corpus, tdm, freqencyCutoff = 1, loadingBar = TRUE) {
  model <- createFreqModel(corpus, tdm, freqencyCutoff = freqencyCutoff, loadingBar = loadingBar)
  
  n4grams <- ngramsFromCorpus(corpus, n = 4)
  n4gramsTable <- data.table(get.phrasetable(n4grams))
  n4gramsTable <- n4gramsTable[freq > freqencyCutoff]
  n4gramsTable[, ngrams := gsub(" $", "", ngrams)]
  
  
  n4gramsTable[, word2 := sub("(['|\n|,|0-9A-Za-z]+)", "", ngrams)]
  n4gramsTable[, word2 := sub("^ ", "", word2)]
  n4gramsTable[, word2 := sub(" .+", "", word2)]
  setkey(n4gramsTable, "word2")
  n4gramsTable <- n4gramsTable[wordFreqTable]
  setnames(n4gramsTable, "index", "indexWord2")
  
  if(loadingBar)
   setTxtProgressBar(pb, 8/loadingSteps)
  
  n4gramsTable[, word3 := gsub("^.+ ", "", ngrams)]
  setkey(n4gramsTable, "word3")
  n4gramsTable <- n4gramsTable[wordFreqTable]
  setnames(n4gramsTable, "index", "indexWord3")
  
  n4gramsTable <- n4gramsTable[!is.na(n4gramsTable$freq)]
  n4gramsTable <- n4gramsTable[order(-freq)]
  n4gramsTable <- n4gramsTable[,c("indexWord1", "indexWord2", "indexWord3"), with = FALSE]
  
  
  
  model
}