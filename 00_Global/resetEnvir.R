# Quick reset of all
resetEnvir <- function(){
  # Source all libraries 
  source("00_Global/libraries.R")
  
  # Create new corpus and models
  corpus <<- createCorpus()
  tdm <<- createTdm()
  
  freqModel <<- createFreqModel(corpus, tdm)
  KNModel <<- createKNModel(corpus, tdm)
  
  "Done"
}

deleteTempObjects <- function(){
  if(file.exists(trainFolder))
    folder <- trainFolder else
      folder <- originalDataFolder
  
  for(file in c("/corpus.RData", "/tdm.RData",
                "/freqModel.RData", "/KNModel.RData")){
    if(file.exists(paste0(folder, file))){
      print(paste("Deleting", paste0(folder, file)))
      file.remove(paste0(folder, file))
    }
  }
  
  "Done"
}