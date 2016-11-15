grep("^a ", table$ngrams)[1:3]

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
freqModel <- function(n2gramsTable, n3gramsTable, word1, word2){
  # Find solutions in the 3-grams table
  sol3gram <- grep(paste0("^", word1, " ", word2, " "), n3gramsTable$ngrams)[1:3]
  
  solutions <- n3gramsTable[sol3gram]$ngrams
  
  solutions <- strip(solutions)
  
  # Find more solutions in the 2-grams table
  findMore <- sum(is.na(sol3gram))
  
  if(findMore > 0){
    sol2gram <- grep(paste0("^", word2, " "), n2gramsTable$ngrams)[1:3]
    
    for(i in 1:findMore){
      # Check if the found solutions has allready been found in the 3-gram before
      k <- i
      while(sol %in% solutions){
        sol <- n2gramsTable[sol2gram]$ngrams[k]
        sol <- strip(sol)
        k <- k + 1
        if(is.na(sol)) break
      }
      solutions[3 - findMore + i] <- sol
    }
  }
  
  # Return found solutions
  solutions
}