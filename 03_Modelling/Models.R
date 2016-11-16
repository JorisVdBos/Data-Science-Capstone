source("01_Load/load.R")
if(!exists("corpus"))
  corpus <- createCorpus()
if(!exists("tdm"))
  tdm <- TermDocumentMatrix(corpus, control = list(wordLengths=c(0, Inf)))

# This function will translate the output of the n-grams to a one-word solution. (Basically it will strip a string to return the last word.)
strip <- function(x){
  output <- character()
  for(i in x){
    i <- gsub(" $", "", i)
    i <- gsub(".+ ", "", i)
    output <- c(output, i)
  }
  
  output
}

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
  
  # Finding input
  wordFreq <- model$wordFreqTable
  n2gramsTable <- model$n2gramsTable
  n3gramsTable <- model$n3gramsTable
  
  # Find solutions in the 3-grams table
  sol3gram <- grep(paste0("^", word1, " ", word2, " "), n3gramsTable$ngrams)[1:3]
  
  solutions <- data.frame(value = n3gramsTable[sol3gram]$ngrams)
  solutions$value <- strip(solutions$value)
  solutions$source <- rep("n3gramsTable", 3)
  
  # Find more solutions in the 2-grams table
  findMore <- sum(is.na(sol3gram))
  
  if(findMore > 0){
    sol2gram <- grep(paste0("^", word2, " "), n2gramsTable$ngrams)
    
    for(i in 1:findMore){
      sol <- n2gramsTable[sol2gram]$ngrams[i]
      sol <- strip(sol)
      
      # Check if the found solutions has allready been found in the 3-gram before
      k <- i
      while(sol %in% solutions$value){
        k <- k + 1
        sol <- n2gramsTable[sol2gram]$ngrams[k]
        sol <- strip(sol)
        if(is.na(sol)) break
      }
      
      # Add 2gram solution to solutions vector
      solutions$value[3 - findMore + i] <- sol
      solutions$source[3 - findMore + i] <- "2gramsTable"
    }
  }
  
  # Find more solutions in the freqency table
  findMore <- sum(is.na(solutions$value))
  
  if(findMore > 0){
    
    for(i in 1:findMore){
      sol <- wordFreq$word[i]
      sol <- strip(sol)
      
      # Check if the found solutions has allready been found in the 3-gram before
      k <- i
      while(sol %in% solutions$value){
        k <- k + 1
        sol <- wordFreq$word[k]
        sol <- strip(sol)
        if(is.na(sol)) break
      }
      
      # Add 2gram solution to solutions vector
      solutions$value[3 - findMore + i] <- sol
      solutions$source[3 - findMore + i] <- "wordFreqencyTable"
    }
  }
  
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
  n2gramsTable <- n2gramsTable[order(-freq)]
  n3grams <- ngramsFromCorpus(corpus, n = 3)
  n3gramsTable <- data.table(get.phrasetable(n3grams))
  n3gramsTable[, ngrams := gsub(" $", "", ngrams)]
  n3gramsTable <- n3gramsTable[order(-freq)]
  
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
  wordFreqTable <- wordFreqTable[,"word", with = FALSE]
  n2gramsTable <- n2gramsTable[,c("indexWord1", "indexWord2"), with = FALSE]
  n3gramsTable <- n3gramsTable[,c("indexWord1", "indexWord2", "indexWord3"), with = FALSE]
  
  # Output
  output <- list(wordFreqTable = wordFreqTable, 
                 n2gramsTable = n2gramsTable, 
                 n3gramsTable = n3gramsTable)
  class(output) <- append(class(output),"FreqModel")
  
  output
}