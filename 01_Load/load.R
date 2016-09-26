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
  con <- file(paste0("RawData/final/", lang, "/", lang, ".blogs.txt", "r"))
  blogTexts <<- readLines(con, lines)
  close(con)
  
  con <- file(paste0("RawData/final/", lang, "/", lang, ".news.txt", "r"))
  newsTexts <<- readLines(con, lines)
  close(con)
  
  con <- file(paste0("RawData/final/", lang, "/", lang, ".twitter.txt", "r"))
  twitterTexts <<- readLines(con, lines)
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
    fileLength <- length(readLines(con))
    close(con)
    readLines <- sample(fileLength, floor(fileLength*sampleSize), replace = FALSE)
    readLines <- readLines[order(readLines)]
    
    # Create the new text file
    file.create(paste0("RawData/sample/", file))
    
    # Read lines and write them in a seperate file
    conR <- file(paste0("RawData/final/", lang, "/", file), "r")
    conW <- file(paste0("RawData/sample/", file), "w")
    print(paste0("Reading ", file,"..."))
    pb <- txtProgressBar(style = 3)
    for(i in 1:fileLength){
      setTxtProgressBar(pb, i/fileLength)
      line <- readLines(conR, 1)
      if(i %in% readLines) writeLines(line, conW)
    }
    close(pb)
    close(conR)
    close(conW)
  }
  
  return("Sample dir created.")
}

# Creating a Corpus
# Tutorial: https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html#loading-texts
# The function takes the data from the folder "RawData/sample" if it exists (and disregard the lang variable). Otherwise it will take directly from the raw data in "RawData/final/en_US/"
createCorpus <- function(lang = "en_US"){
  print("Creating Corpus object.")
  
  if(file.exists("RawData/sample"))
    corpus <<- Corpus(DirSource("RawData/sample")) else
      corpus <<- Corpus(DirSource("RawData/final/", lang, "/"))
  ## removing punctuations
  corpus <- tm_map(corpus, removePunctuation)
  ## Converting all to lowercase
  corpus <- tm_map(corpus, tolower)
  ## Removing numbers
  corpus <- tm_map(corpus, removeNumbers)
  ## Removing stopwords
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  ## Removing common word endings
  corpus <- tm_map(corpus, stemDocument)
  ## Removing white space
  corpus <- tm_map(corpus, stripWhitespace)
  
  ## Make the corpus a text document
  corpus <- tm_map(corpus, PlainTextDocument) 
  
  ## Construct a term-document Matrix
  print("Constructing the term-document Matrix.")
  tdm <<- TermDocumentMatrix(corpus)
  
  "Done!"
}
