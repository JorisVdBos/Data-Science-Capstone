source("01_Load/load.R")
source("02_ExploratoryAnalysis/Explore.R")
source("03_Modelling/General.R")

# This first model looks at the freqency table of the 3-grams and takes the three options that are most frequent. If less than three options are found, it will look at the 2-grams table
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
