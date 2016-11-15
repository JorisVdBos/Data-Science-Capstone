source("01_Load/load.R")
if(!exists("corpus"))
  createCorpus()

# This function will translate the output of the n-grams to a one-word solution. (Basically it will strip a string to return the last word.)
strip <- function(x){
  output <- character()
  for(i in x){
    i <- gsub(" $", "", i)
    i <- gsub(".+ ", "", i)
    output <- c(output, i)
  }
  
  output
}

# This first model looks at the freqency table of the 3-grams and takes the three options that are most frequent. If less than three options are found, it will look at the 2-grams table
freqModel <- function(wordFreq, n2gramsTable, n3gramsTable, word1, word2){
  # Find solutions in the 3-grams table
  sol3gram <- grep(paste0("^", word1, " ", word2, " "), n3gramsTable$ngrams)[1:3]
  
  solutions <- data.frame(value = n3gramsTable[sol3gram]$ngrams)
  solutions$value <- strip(solutions$value)
  solutions$source <- rep("n3gramsTable", 3)
  
  # Find more solutions in the 2-grams table
  findMore <- sum(is.na(sol3gram))
  
  if(findMore > 0){
    sol2gram <- grep(paste0("^", word2, " "), n2gramsTable$ngrams)
    
    for(i in 1:findMore){
      sol <- n2gramsTable[sol2gram]$ngrams[i]
      sol <- strip(sol)
      
      # Check if the found solutions has allready been found in the 3-gram before
      k <- i
      while(sol %in% solutions$value){
        k <- k + 1
        sol <- n2gramsTable[sol2gram]$ngrams[k]
        sol <- strip(sol)
        if(is.na(sol)) break
      }
      
      # Add 2gram solution to solutions vector
      solutions$value[3 - findMore + i] <- sol
      solutions$source[3 - findMore + i] <- "2gramsTable"
    }
  }
  
  # Find more solutions in the freqency table
  findMore <- sum(is.na(solutions$value))
  
  if(findMore > 0){
    
    for(i in 1:findMore){
      sol <- wordFreq$word[i]
      sol <- strip(sol)
      
      # Check if the found solutions has allready been found in the 3-gram before
      k <- i
      while(sol %in% solutions$value){
        k <- k + 1
        sol <- wordFreq$word[k]
        sol <- strip(sol)
        if(is.na(sol)) break
      }
      
      # Add 2gram solution to solutions vector
      solutions$value[3 - findMore + i] <- sol
      solutions$source[3 - findMore + i] <- "wordFreqencyTable"
    }
  }
  
  # Return solutions vector
  solutions
}