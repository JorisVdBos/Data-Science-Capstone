source("01_Load/load.R")
source("02_ExploratoryAnalysis/Explore.R")
source("03_Modelling/General.R")

# This first model looks at the freqency table of the 3-grams and takes the three options that are most frequent. If less than three options are found, it will look at the 2-grams table
createFreqModel <- function(corpus, tdm = NULL, freqencyCutoff = 1, loadingBar = TRUE) {
  
  if(is.null(tdm)){
    print("creating tdm...")
    tdm <- TermDocumentMatrix(corpus, control = list(wordLengths=c(0, Inf)))
    print("Done!")
  }
  
  loadingSteps <- 10
  if(loadingBar)
    pb <- txtProgressBar(style = 3)
  
  # Create tables
  wordFreqTable <- wordFreq(tdm)
  wordFreqTable <- wordFreqTable[Encoding(wordFreqTable$word) != "UTF-8"]
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

# Prediction function
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