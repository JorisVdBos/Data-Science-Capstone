source(file = "03_Modelling/Models.R")

messagesTesting <- FALSE

# Reads random sentences in the data and determines how many words could be predicted
testModel <- function(model, fraction = 0.1, seed = 1, lang = "en_US"){
  set.seed(seed)
  if(fraction > 1 || fraction < 0) iconv <- 1
  
  # Init
  score <- data.frame(totalInputs = c(0, 0, 0, 0), recommendedInputs = c(0, 0, 0, 0))
  row.names(score) <- c(paste0(lang, c(".blogs.txt", ".news.txt", ".twitter.txt")), "Total")
  recommended <- character()
  notRecommended <- character()
  
  for(file in paste0(lang, c(".blogs.txt", ".news.txt", ".twitter.txt"))){
    # Calculate which lines to read
    con <- file(paste0("RawData/sampleTrain/", file), "r")
    fileLengthTrain <- length(readLines(con, encoding="UTF-8"))
    close(con)
    
    print(paste("Total training lines for", file, "was", fileLengthTrain))
    
    con <- file(paste0("RawData/sampleTest/", file), "r")
    fileLengthTest <- length(readLines(con))
    close(con)
    fileLengthTestSample <- sample(1:fileLengthTest, size = fileLengthTest*fraction, replace = FALSE)
    
    print(paste("Total testing lines for", file, "is", length(fileLengthTestSample)))
    
    # Read lines and write them in a seperate file
    conR <- file(paste0("RawData/sampleTest/", file), "r")
    
    if(messagesTesting)
      pb <- txtProgressBar(style = 3)
    
    for(i in 1:fileLengthTest){
      if(messagesTesting)
        setTxtProgressBar(pb, i/fileLengthTest)
      line <- readLines(conR, 1)
      if(i %in% fileLengthTestSample){
        # Split the line
        if(messagesTesting)
          print(line)
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
          
          if(messagesTesting)
            print(paste("trying to predict", word, "from words", word1, "and", word2))
          
          predicted <- predict(model, word1, word2)$value
          
          if(messagesTesting){
            print(paste("The options were"))
            print(predicted)
          }
          
          if(word %in% predicted){
            if(messagesTesting)
              print("Prediction was a success!")
            score[file, "recommendedInputs"] <- score[file, "recommendedInputs"]  + 1
            score["Total", "recommendedInputs"] <- score["Total", "recommendedInputs"]  + 1
          } else {
            if(messagesTesting)
              print("The predicted words did not contain the word.")
          }
          if(messagesTesting)
            print(" ")
        }
      }
    }
    
    if(messagesTesting)
      close(pb)
    close(conR)
  }
  
  score$percentageRecommended <- score$recommendedInputs / score$totalInputs
  
  names(score) <- c("Words attempted to be predicted", "Prediction was a success", "Percentage successful preduction")
  return(score)
}