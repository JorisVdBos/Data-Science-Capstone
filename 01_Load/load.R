source("00_Global/libraries.R")
source("00_Global/settings.R")

# Downloading the files
downloadFiles <- function(){
  download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "data.zip")
  if(!file.exists("RawData")) dir.create("RawData")
  unzip("data.zip", exdir = "RawData")
  file.remove("data.zip")
}

# Loading the files into R
readTextsSample <- function(lines = 10, lang = "en_US"){
  con <- file(paste0("RawData/final/", lang, "/", lang, ".blogs.txt"), "r")
  blogTexts <<- readLines(con, lines, encoding="UTF-8")
  close(con)
  
  con <- file(paste0("RawData/final/", lang, "/", lang, ".news.txt"), "r")
  newsTexts <<- readLines(con, lines, encoding="UTF-8")
  close(con)
  
  con <- file(paste0("RawData/final/", lang, "/", lang, ".twitter.txt"), "r")
  twitterTexts <<- readLines(con, lines, encoding="UTF-8")
  close(con)
}

# Creating a subsample dir of the data. If sampleSize = 1 is chosen, it will simply delete the sample directory, as it would be a complete copy of the raw data
createSampleDataDir <- function(sampleSize = 1, seed = 1, lang = "en_US"){
  if(file.exists("RawData/sample")) rm("RawData/sample")
  set.seed(seed)
  
  # Check sampleSize parameter
  if(is.numeric(sampleSize) && (sampleSize >= 1 || sampleSize <= 0)) 
    return("Deleted sample map.")
  
  # Create sample dir
  dir.create("RawData/sample")
  
  for(file in paste0(lang, c(".blogs.txt", ".news.txt", ".twitter.txt"))){
    # Calculate which lines to read
    con <- file(paste0("RawData/final/", lang, "/", file), "r")
    fileLength <- length(readLines(con, encoding="UTF-8"))
    close(con)
    linesToRead <- sample(fileLength, floor(fileLength*sampleSize), replace = FALSE)
    linesToRead <- linesToRead[order(linesToRead)]
    
    # Create the new text file
    file.create(paste0("RawData/sample/", file))
    
    # Read lines and write them in a seperate file
    conR <- file(paste0("RawData/final/", lang, "/", file), "r")
    conW <- file(paste0("RawData/sample/", file), "w")
    print(paste0("Reading ", file,"..."))
    pb <- txtProgressBar(style = 3)
    for(i in 1:fileLength){
      setTxtProgressBar(pb, i/fileLength)
      line <- readLines(conR, 1, encoding="UTF-8")
      if(i %in% linesToRead) writeLines(line, conW)
    }
    close(pb)
    close(conR)
    close(conW)
  }
  
  return("Sample dir created.")
}

# Creating a Corpus and term-document matrix
# Tutorial: https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html#loading-texts
# The function takes the data from the folder "RawData/sample" if it exists (and disregard the lang variable). Otherwise it will take directly from the raw data in "RawData/final/en_US/"
createCorpus <- function(lang = "en_US"){
  print("Creating Corpus object.")
  
  if(file.exists("RawData/sample"))
    dirSource <- DirSource("RawData/sample") else
      dirSource <- DirSource("RawData/final/", lang, "/")
  
  dirSource$encoding <- "UTF-8"
  corpus <- Corpus(dirSource)
    
  ## Removing numbers
  corpus <- tm_map(corpus, removeNumbers)
  ## Punctuations: Dots will be the next line. Comma's can be interperted as a word
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\. *", replacement = "\n")
  removePunctuationsExeptions <- function(x) {
    x <- gsub(",+", "123", x)
    x <- gsub("'+", "456", x)
    x <- gsub("[[:punct:]]+", "", x)
    x <- gsub(" *123 *", " , ", x)
    x <- gsub(" *456 *", "'", x)
    x
  }
  corpus <- tm_map(corpus, content_transformer(removePunctuationsExeptions))
    
  ## Removing double \n and \n at the beginning and end and putting it between spaces
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = " *[\n]+ *", replacement = " \n ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "^( \n) | (\n )$", replacement = " ")
  
  ## Converting all to lowercase
  corpus <- tm_map(corpus, tolower)
  
  ## Make the corpus a text document
  corpus <- tm_map(corpus, PlainTextDocument) 
  
  print("Done!")
  
  corpus
}
