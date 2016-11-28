if(!exists("loadedAllScripts"))
  source("00_Global/libraries.R")


# Reads random sentences in the data and determines how many words could be predicted
testModel <- function(model, fraction = 0.1, seed = 1, validate = FALSE, loadingBar = TRUE){
  set.seed(seed)
  
  if(fraction > 1 || fraction < 0) fraction <- 1
  
  # Init
  score <- data.frame(totalInputs = c(0, 0, 0, 0), recommendedInputs = c(0, 0, 0, 0))
  row.names(score) <- c(dataFiles, "Total")
  recommended <- character()
  notRecommended <- character()
  wordAccuracy <- data.frame()
  
  for(i in 1:giveNumberOfPossibilities){
    if(i == 1){
      wordAccuracy <- data.frame("word1" = 0)
    } else {
      tempDF <- data.frame(0)
      names(tempDF)[1] = paste0("word", i)
      wordAccuracy <- cbind(wordAccuracy, tempDF)
    }
  }
  
  for(file in dataFiles){
    
    # Calculate which lines to read
    con <- file(paste0(trainFolder, "/", file), "r")
    fileLengthTrain <- length(readLines(con, encoding="UTF-8"))
    close(con)
    
    print(paste("Total training lines for", file, "was", fileLengthTrain))
    
    if(validate)
      con <- file(paste0(validationFolder, "/", file), "r") else
        con <- file(paste0(testFolder, "/", file), "r")
    fileLengthTest <- length(readLines(con))
    close(con)
    fileLengthTestSample <- sample(1:fileLengthTest, size = fileLengthTest*fraction, replace = FALSE)
    
    if(validate)
      print(paste("Total validation lines for", file, "is", length(fileLengthTestSample))) else
        print(paste("Total testing lines for", file, "is", length(fileLengthTestSample)))
    
    # Read lines from testing/validation
    if(validate)
      conR <- file(paste0(validationFolder, "/", file), "r") else
        conR <- file(paste0(testFolder, "/", file), "r")
    
    if(loadingBar)
      pb <- txtProgressBar(style = 3)
    
    for(i in 1:fileLengthTest){
      if(loadingBar)
        setTxtProgressBar(pb, i/fileLengthTest)
      line <- readLines(conR, 1)
      if(i %in% fileLengthTestSample){
        # Split the line
        line <- iconv(line, to = "latin1")
        line <- modelInput(line, mode = "testing")
        score["Total", "totalInputs"] <- score["Total", "totalInputs"]  + length(line)
        score[file, "totalInputs"] <- score[file, "totalInputs"]  + length(line)
        
        if(length(line) > 0)
        for(i in 1:length(line)){
          word <- line[i]
          word1 <- line[i-2]
          word2 <- line[i-1]
          
          if(i == 1) {
              word1 <- "\n"
              word2 <- "\n"
            } else
          if(i == 2){
            word1 <- word2
            word2 <- "\n"
          }
          
          # print(paste("trying to predict", word, "from words", word1, "and", word2))
          
          predicted <- predict(model, word1, word2)$value
          
          # print(paste("The options were"))
          # print(predicted)
          
          if(word %in% predicted){
            #print("Prediction was a success!")
            correctWord <- which(word == predicted)
            if(length(correctWord) == 1)
              wordAccuracy[[paste0("word", correctWord)]] <-
                wordAccuracy[[paste0("word", correctWord)]] + 1
            
            score[file, "recommendedInputs"] <- score[file, "recommendedInputs"]  + 1
            score["Total", "recommendedInputs"] <- score["Total", "recommendedInputs"]  + 1
          } else {
            # No extra counts
            #print("The predicted words did not contain the word.")
          }
        }
      }
    }
    
    if(loadingBar)
      close(pb)
    close(conR)
  }
  
  score$percentageRecommended <- score$recommendedInputs / score$totalInputs
  wordAccuracy <- wordAccuracy / score["Total", "totalInputs"]
  
  names(score) <- c("Words total", "Prediction success", "% successful prediction")
  
  return(list(score, wordAccuracy))
}