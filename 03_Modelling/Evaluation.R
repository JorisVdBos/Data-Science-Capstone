# Reads random sentences in the data and determines how many words could be predicted
testModel <- function(fraction = 0.1, seed = 1, lang = "en_US"){
  set.seed(seed)
  if(fraction > 1 || fraction < 0) fraction <- 1
  
  for(file in paste0(lang, c(".blogs.txt", ".news.txt", ".twitter.txt"))){
    # Calculate which lines to read
    con <- file(paste0("RawData/sampleTrain/", file), "r")
    fileLengthTrain <- length(readLines(con, encoding="UTF-8"))
    close(con)
    
    print(paste("Total training lines for", file, "was", fileLengthTrain))
    
    con <- file(paste0("RawData/sampleTest/", file), "r")
    fileLengthTest <- length(readLines(con, encoding="UTF-8"))
    close(con)
    
    fileLengthTest <- sample(1:fileLengthTest, size = fileLengthTest*fraction, replace = FALSE)
    
    print(paste("Total testing lines for", file, "is", length(fileLengthTest)))
    
    # Read lines and write them in a seperate file
    conR <- file(paste0("RawData/sampleTest/", file), "r")
    
    print(paste0("Reading ", file,"..."))
    pb <- txtProgressBar(style = 3)
    for(i in 1:fileLength){
      setTxtProgressBar(pb, i/fileLength)
      line <- readLines(conR, 1, encoding="UTF-8")
      
      # Split the line
      print(line)
      line <- modelInput(line, mode = "testing")
    }
    
    close(pb)
    close(conR)
  }
  
  return("Test.")
}