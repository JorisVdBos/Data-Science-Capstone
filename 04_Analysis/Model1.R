# Model created from 10% data
giveNumberOfPossibilities <- 3
pred <- testModel(freqModel, fraction = 0.001, loadingBar = TRUE)

wordFreqencyTable <- freqModel$wordFreqTable
wordFreqencyTable[, index := 1:length(wordFreqencyTable$word)]
predicted <- pred$predictedWords[order(-N)]
notPredicted <- pred$notPredictedWords[order(-N)]

setkey(wordFreqencyTable, "word")
setkey(predicted, "predictedWords")
predicted <- predicted[wordFreqencyTable]
predicted <- predicted[!is.na(predicted$N)]
predicted <- predicted[order(-N)]
predicted <- predicted[1:100]

setkey(notPredicted, "notPredictedWords")
notPredicted <- notPredicted[wordFreqencyTable]
notPredicted <- notPredicted[!is.na(notPredicted$N)]
notPredicted <- notPredicted[order(-N)]

notPredicted <- notPredicted[1:100]

# Analysis
summary(predicted$index)
summary(notPredicted$index)
