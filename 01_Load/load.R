if(!exists("loadedAllScripts"))
  source("00_Global/libraries.R")

# Downloading the files
downloadFiles <- function(){
  if(!file.exists(originalDataFolder)){
    download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip", "data.zip")
    if(!file.exists(rawDataFolder)) dir.create(rawDataFolder)
    unzip("data.zip", exdir = rawDataFolder)
    file.remove("data.zip")
  }
}


# Creating a subsample dir of the data. If sampleSize = 1 is chosen, it will simply delete the sample directory, as it would be a complete copy of the raw data
createSampleDataDir <- function(sampleSize = 1, seed = 1){
  
  if(file.exists(trainFolder)) rm(trainFolder)
  if(file.exists(testFolder)) rm(testFolder)
  if(file.exists(validateFolder)) rm(validateFolder)
  set.seed(seed)
  
  # Check sampleSize parameter
  if(is.numeric(sampleSize) && (sampleSize >= 1 || sampleSize <= 0)) 
    return("Deleted sample map.")
  
  # Create sample dir
  dir.create(trainFolder)
  dir.create(testFolder)
  dir.create(validateFolder)
  
  for(file in dataFiles){
    # Calculate which lines to read
    con <- file(paste0(originalDataFolder, "/", file), "r")
    fileLength <- length(readLines(con, encoding="UTF-8"))
    close(con)
    linesToReadTrain <- sample(fileLength, floor(fileLength*sampleSize), replace = FALSE)
    
    print(paste("Total training lines for", file, "is", length(linesToReadTrain)))
    
    linesToReadTest <- which(!1:fileLength %in% linesToReadTrain)
    divisionTestValidate <- sample(2, size = length(linesToReadTest), replace = TRUE)
    linesToReadValidate <- linesToReadTest[divisionTestValidate == 1]
    linesToReadTest <- linesToReadTest[divisionTestValidate == 2]
    
    if(length(linesToReadTest) > length(linesToReadTrain)){
      linesToReadTest <- sample(linesToReadTest, size = length(linesToReadTrain), replace = FALSE)
      linesToReadValidate <- sample(linesToReadValidate, size = length(linesToReadTrain), replace = FALSE)
    }
    
    print(paste("Total testing lines for", file, "is", length(linesToReadTest)))
    print(paste("Total validation lines for", file, "is", length(linesToReadValidate)))
    
    # Create the new text file
    file.create(paste0(trainFolder, "/", file))
    file.create(paste0(testFolder, "/", file))
    file.create(paste0(valdateFolder, "/", file))
    
    # Read lines and write them in a seperate file
    conR <- file(paste0(originalDataFolder, "/", file), "r")
    conWTrain <- file(paste0(trainFolder, "/", file), "w")
    conWTest <- file(paste0(testFolder, "/", file), "w")
    conWValidate <- file(paste0(valdateFolder, "/", file), "w")
    
    print(paste0("Reading ", file,"..."))
    pb <- txtProgressBar(style = 3)
    for(i in 1:fileLength){
      setTxtProgressBar(pb, i/fileLength)
      line <- readLines(conR, 1, encoding="UTF-8")
      if(i %in% linesToReadTrain) writeLines(line, conWTrain)
      if(i %in% linesToReadTest) writeLines(line, conWTest)
      if(i %in% linesToReadValidate) writeLines(line, conWValidate)
    }
    close(pb)
    close(conR)
    close(conWTrain)
    close(conWTest)
    close(conWValidate)
  }
  
  return("Sample dir created.")
}

# The corpus filter is defined here, so it can be used later to craft the input into the model
removePunctuationsExeptions <- function(x) {
  x <- gsub(",+", "123", x)
  x <- gsub("'+", "456", x)
  x <- gsub("[[:punct:]]+", "", x)
  x <- gsub(" *123 *", " , ", x)
  x <- gsub(" *456 *", "'", x)
  x
}
corpusFilter <- function(corpus){
  ## Removing numbers
  corpus <- tm_map(corpus, removeNumbers)
  ## Punctuations: Dots will be the next line. Comma's can be interperted as a word
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\. *", replacement = "\n")
  corpus <- tm_map(corpus, content_transformer(removePunctuationsExeptions))
  
  ## Removing double \n and \n at the beginning and end and putting it between spaces
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = " *[\n]+ *", replacement = " \n ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "^( \n) | (\n )$", replacement = " ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = " +", replacement = " ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = " $", replacement = "")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "^ ", replacement = "")
  
  # Some special character cases
  s <- "\u0091"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "'")
  s <- "\u0092"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "'")
  s <- "\u0093"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "")
  s <- "\u0094"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "")
  
  
  ## Converting all to lowercase
  corpus <- tm_map(corpus, tolower)
  
  corpus
}

# Creating a Corpus and term-document matrix
# Tutorial: https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html#loading-texts
# The function takes the data from the folder "sampleTrain" if it exists. Otherwise it will take directly from the raw data in "final/en_US/". See the paths file in the global map for the full paths
createCorpus <- function(){
  
  print("Creating Corpus object.")
  
  if(file.exists(trainFolder))
    dirSource <- DirSource(trainFolder) else
      dirSource <- DirSource(originalDataFolder)
  
  dirSource$encoding <- "UTF-8"
  corpus <- Corpus(dirSource)
  
  # See above
  corpus <- corpusFilter(corpus)
  
  ## Make the corpus a text document
  corpus <- tm_map(corpus, PlainTextDocument) 
  
  print("Done!")
  
  corpus
}
