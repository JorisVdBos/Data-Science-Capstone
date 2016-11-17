source("01_Load/load.R")
source("02_ExploratoryAnalysis/Explore.R")
source("03_Modelling/General.R")

if(!exists("corpus"))
  corpus <- createCorpus()
if(!exists("tdm"))
  tdm <- TermDocumentMatrix(corpus, control = list(wordLengths=c(0, Inf)))

# This first model looks at the freqency table of the 3-grams and takes the three options that are most frequent. If less than three options are found, it will look at the 2-grams table
predict.FreqModel <- function(model, word1 = NULL, word2 = NULL){
  # If word1 is empty, regard the input as the beginning of a sentence:
  if(is.null(word1)){
    word1 <- "/n"
  }
  
  # If word2 is empty, regard the input as a first word of a sentence:
  if(is.null(word2)){
    word2 <- word1
    word1 <- "/n"
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
      while(sol %in% solutions$value){
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
      while(k %in% solutions$value){
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
createFreqModel <- function(corpus, tdm) {
  # Create tables
  wordFreqTable <- wordFreq(tdm)
  wordFreqTable <- wordFreqTable[order(-freq)]
  n2grams <- ngramsFromCorpus(corpus, n = 2)
  n2gramsTable <- data.table(get.phrasetable(n2grams))
  n2gramsTable[, ngrams := gsub(" $", "", ngrams)]
  n3grams <- ngramsFromCorpus(corpus, n = 3)
  n3gramsTable <- data.table(get.phrasetable(n3grams))
  n3gramsTable[, ngrams := gsub(" $", "", ngrams)]
  
  # Converting the words to numbers
  wordFreqTable[, index := 1:length(wordFreqTable$word)]
  wordFreqTable <- rbindlist(list(wordFreqTable, data.frame("\n", 0, 0)))
  wordFreqTable$word <- as.character(wordFreqTable$word)
  setkey(wordFreqTable, "word")
  
  n2gramsTable[, word1 := gsub(" .+", "", ngrams)]
  setkey(n2gramsTable, "word1")
  n2gramsTable <- n2gramsTable[wordFreqTable]
  setnames(n2gramsTable, "index", "indexWord1")
  
  n2gramsTable[, word2 := gsub(".+ ", "", ngrams)]
  setkey(n2gramsTable, "word2")
  n2gramsTable <- n2gramsTable[wordFreqTable]
  setnames(n2gramsTable, "index", "indexWord2")
  
  n3gramsTable[, word1 := gsub(" .+", "", ngrams)]
  setkey(n3gramsTable, "word1")
  n3gramsTable <- n3gramsTable[wordFreqTable]
  setnames(n3gramsTable, "index", "indexWord1")
  
  n3gramsTable[, word2 := sub("(['|\n|,|0-9A-Za-z]+)", "", ngrams)]
  n3gramsTable[, word2 := sub("^ ", "", word2)]
  n3gramsTable[, word2 := sub(" .+", "", word2)]
  setkey(n3gramsTable, "word2")
  n3gramsTable <- n3gramsTable[wordFreqTable]
  setnames(n3gramsTable, "index", "indexWord2")
  
  n3gramsTable[, word3 := gsub("^.+ ", "", ngrams)]
  setkey(n3gramsTable, "word3")
  n3gramsTable <- n3gramsTable[wordFreqTable]
  setnames(n3gramsTable, "index", "indexWord3")
  
  # Reduce size by dropping all columns except the Index
  wordFreqTable <- wordFreqTable[order(-freq)]
  wordFreqTable <- wordFreqTable[,"word", with = FALSE]
  n2gramsTable <- n2gramsTable[order(-freq)]
  n2gramsTable <- n2gramsTable[,c("indexWord1", "indexWord2"), with = FALSE]
  n3gramsTable <- n3gramsTable[order(-freq)]
  n3gramsTable <- n3gramsTable[,c("indexWord1", "indexWord2", "indexWord3"), with = FALSE]
  
  # Output
  output <- list(wordFreqTable = wordFreqTable, 
                 n2gramsTable = n2gramsTable, 
                 n3gramsTable = n3gramsTable)
  class(output) <- append(class(output),"FreqModel")
  
  output
}