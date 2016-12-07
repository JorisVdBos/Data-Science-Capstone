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
  set.seed(seed)
  
  if(file.exists(trainFolder)) rm(trainFolder)
  if(file.exists(testFolder)) rm(testFolder)
  if(file.exists(validateFolder)) rm(validateFolder)
  percentageMap <- paste0(paste0(tempDataFolder, "/percentage", usePercentageOfData*100))
  if(!file.exists(percentageMap)) dir.create(percentageMap)
  
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
    file.create(paste0(validateFolder, "/", file))
    
    # Read lines and write them in a seperate file
    conR <- file(paste0(originalDataFolder, "/", file), "r")
    conWTrain <- file(paste0(trainFolder, "/", file), "w")
    conWTest <- file(paste0(testFolder, "/", file), "w")
    conWValidate <- file(paste0(validateFolder, "/", file), "w")
    
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
  # Replace/"save" punctuations to be disgarded, in numbers
  x <- gsub("[0-9]+", "#", x)
  x <- gsub("[#*\\.*|#*,*]*#", "000", x) # A number in any notation
  x <- gsub(",+", " 001 ", x) # Comma
  x <- gsub("\\.+", " 002 ", x) # Point
  x <- gsub("'+", "003", x) # Accent -> Not seperated by spaces to conserve words such as "don't" etc.
  x <- gsub("\\?+", " 004 ", x) # QuestionMark
  x <- gsub("!+", " 005 ", x) # ExclamationMark
  
  # Remove all punctuations
  x <- gsub("[[:punct:]]+", "", x)
  
  # Re-install the saved punctuations
  x <- gsub("( *000)+ *", " # ", x) 
  x <- gsub(" *001 *", " , ", x)
  x <- gsub(" *002 *", " \n ", x) # Dot is replaced by a New line
  x <- gsub("(003)+", "'", x)
  x <- gsub(" *004 *", " ? ", x)
  x <- gsub(" *005 *", " ! ", x)
  
  # Finally, convert to to lower case
  tolower(x)
}

corpusFilter <- function(corpus){
  ## Punctuations: Dots will be the next line. Comma's can be interperted as a word, see above
  corpus <- tm_map(corpus, content_transformer(removePunctuationsExeptions))
  
  ## Removing double \n and \n at the beginning and end and putting it between spaces
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "$", replacement = " \n")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = "^", replacement = "\n ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = " *(\n *)+", replacement = " \n ")
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = " +", replacement = " ")
  
  # Some special character cases
  s <- "\u0091"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "'")
  s <- "\u0092"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "'")
  s <- "\u0093"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "")
  s <- "\u0094"
  corpus <- tm_map(corpus, content_transformer(gsub), pattern = s, replacement = "")
  
  corpus
}

# Creating a Corpus and term-document matrix
# Tutorial: https://rstudio-pubs-static.s3.amazonaws.com/31867_8236987cf0a8444e962ccd2aec46d9c3.html#loading-texts
# The function takes the data from the folder "sampleTrain" if it exists. Otherwise it will take directly from the raw data in "final/en_US/". See the paths file in the global map for the full paths
createCorpus <- function(){
  
  if(file.exists(trainFolder))
    dirSource <- DirSource(trainFolder) else
      dirSource <- DirSource(originalDataFolder)
  
  dirSource$encoding <- "latin1"
  
  print("Creating Corpus object.")
  corpus <- Corpus(dirSource)
  print("Creating Corpus object complete.")
  
  
  # See above
  print("Structuring Corpus.")
  corpus <- corpusFilter(corpus)
  
  ## Make the corpus a text document
  corpus <- tm_map(corpus, PlainTextDocument) 
  
  print("Done!")
  
  corpus
}
