## Useful functions:
#' findFreqTerms(tdm, 2, 3)
#' findAssocs(tdm, "oil", 0.7)
#' inspect(corpus)
#' meta(crude[[1]])
#' meta(crude[[1]], tag = "topics")
#' plot(tdm, corThreshold = 0.2, weighting = TRUE)
#' termFreq(crude[[14]])

if(!exists("loadedAllScripts"))
  source("00_Global/libraries.R")

# Create table with word freqencies
wordFreq <- function(tdm){
  capture.output(FreqMat <- data.frame(word = rownames(inspect(tdm)), freq = rowSums(inspect(tdm))), file='NUL')
  row.names(FreqMat) <- NULL
  FreqMat <-  data.table(FreqMat)
  FreqMat$word <- as.character(FreqMat$word)
  FreqMat[order(-freq)]
}

# Create n-grams
ngramsFromCorpus <- function(corpus, n = 2) {
  allText <- paste(c(corpus[[1]]$content,
                     corpus[[2]]$content,
                     corpus[[3]]$content), collapse = " \n ")
  ngrams <- ngram(allText, n = n)
  ngrams
}

# Getting a sample from the files into R
readTextsSample <- function(lines = 10, seed = 1){
  set.seed(seed)
  
  sampleTexts <- list()
  
  for(file in dataFiles){
    # Get amount of lines
    con <- file(paste0(trainFolder, "/", file), "r")
    fileLength <- length(readLines(con))
    close(con)
    
    # Sample
    lineSamples <- sample(1:fileLength, size = lines, replace = FALSE)
    
    # Read the sampled lines
    sampleLines <- character()
    con <- file(paste0(trainFolder, "/", file), "r")
    for(i in 1:fileLength){
      lineRead <- readLines(con, 1)
      if(i %in% lineSamples)
        sampleLines <- c(sampleLines, lineRead)
    }
    close(con)
    
    # Accumulate results
    sampleTexts[[file]] <- sampleLines
    
  }
  
  # Return results
  sampleTexts
}

# Create data table with word count and line count
probeData <- function(){
  
  # Init
  files <- character()
  linesTotals <- integer()
  wordsTotals <- integer()
  charTotals <- integer()
  longestLinesW <- integer()
  longestLinesC <- integer()
  
  for(file in list.files(originalDataFolder)){
    
    # Get amount of lines
    con <- file(paste0(originalDataFolder, "/", file), "r")
    fileLength <- length(readLines(con))
    close(con)
    
    # Read lines
    longestLineW <- 0
    longestLineC <- 0
    wordsTotal <- 0
    charTotal <- 0
    con <- file(paste0(originalDataFolder, "/", file), "r")
    for(i in 1:fileLength){
      lineRead <- readLines(con, 1)
      
      words <- length(strsplit(lineRead,' ')[[1]])
      wordsTotal <- wordsTotal + words
      charTotal <- charTotal + nchar(lineRead)
      if(words > longestLineW)
        longestLineW <- words
      if(nchar(lineRead) > longestLineC)
        longestLineC <- nchar(lineRead)
    }
    close(con)
    
    # Accumulate results
    files <- c(files, file)
    linesTotals <- c(linesTotals, fileLength)
    wordsTotals <- c(wordsTotals, wordsTotal)
    charTotals <- c(charTotals, charTotal)
    longestLinesW <- c(longestLinesW, longestLineW)
    longestLinesC <- c(longestLinesC, longestLineC)
    
  }
  
  # Return results
  textFiles <- data.frame(File = files, Lines = linesTotals, Words = wordsTotals,
             Characters = charTotals, longestLineW = longestLinesW, longestLineC = longestLinesC)
  names(textFiles)[5] <- "Longest Line: Words"
  names(textFiles)[6] <- "Longest Line: Characters"
  
  textFiles
}