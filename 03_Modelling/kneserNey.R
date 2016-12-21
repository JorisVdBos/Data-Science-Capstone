# This model follows the Kneser-Ney smooting
# Ref: http://u.cs.biu.ac.il/~yogo/courses/mt2013/papers/chen-goodman-99.pdf
createKNModel <- function(corpus, tdm = NULL, freqencyCutoff = 1, loadingBar = TRUE) {
  
  
  # Check previously made model
  if(file.exists(trainFolder))
    folder <- trainFolder else
      folder <- originalDataFolder
    
  if(file.exists(paste0(folder, "/KNModel.RData"))){
    print(paste("Loading KNModel object from", paste0(folder, "/KNModel.RData")))
    load(paste0(folder, "/KNModel.RData"))
    
    return(KNModel)
  }
  
  print("Creating Kneser-Ney model.")
    
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
  wordFreqTable <- wordFreqTable[freq > freqencyCutoff]
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
  
  # Counting the EOS
  totalEOS <- sum(n2gramsTable[indexWord1 == 0]$freq) + sum(n2gramsTable[indexWord2 == 0]$freq)
  wordFreqTable[index == 0]$freq <- totalEOS
  
  # Calculating the 1-gram probabilities
  total2grams <- dim(n2gramsTable)[1]
  part2grams <- sapply(wordFreqTable$index, function(x) dim(n2gramsTable[indexWord2== x])[1])
  wordFreqTable$prob <- part2grams/total2grams
  
  # Reduce size by dropping all columns except the Index
  wordFreqTable <- wordFreqTable[order(-freq)]
  wordFreqTable <- wordFreqTable[c(2:(dim(wordFreqTable)[1]), 1)] # Putting "\n" last
  n2gramsTable <- n2gramsTable[!is.na(n2gramsTable$freq)]
  n2gramsTable <- n2gramsTable[order(-freq)]
  n2gramsTable <- n2gramsTable[,c("indexWord1", "indexWord2", "freq"), with = FALSE]
  n3gramsTable <- n3gramsTable[!is.na(n3gramsTable$freq)]
  n3gramsTable <- n3gramsTable[order(-freq)]
  n3gramsTable <- n3gramsTable[,c("indexWord1", "indexWord2", "indexWord3", "freq"), with = FALSE]
  
  if(loadingBar)
    setTxtProgressBar(pb, 10/loadingSteps)
  
  # Output
  KNModel <- list(wordFreqTable = wordFreqTable, 
                 n2gramsTable = n2gramsTable, 
                 n3gramsTable = n3gramsTable)
  class(KNModel) <- append(class(KNModel),"KNModel")
  
  if(loadingBar)
    close(pb)
  
  save(KNModel, file = paste0(folder, "/KNModel.RData"))
  return(KNModel)
}

# Prediction function
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