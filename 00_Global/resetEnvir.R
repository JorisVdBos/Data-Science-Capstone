# Quick reset of all
resetEnvir <- function(setPercData = NULL){
  # Source all libraries 
  source("00_Global/libraries.R")
  
  if(!is.null(setPercData)){
    usePercentageOfData <<- setPercData
    trainFolder <<- paste0(tempDataFolder, "/percentage", usePercentageOfData*100, "/sampleTrain")
    testFolder <<- paste0(tempDataFolder, "/percentage", usePercentageOfData*100, "/sampleTest")
    validateFolder <<- paste0(tempDataFolder, "/percentage", usePercentageOfData*100, "/sampleValidate")
  }
  
  # Download files
  downloadFiles()
  
  # Check sample dir
  createSampleDataDir(usePercentageOfData, seed)
  
  # Create new corpus and models
  corpus <<- createCorpus()
  tdm <<- createTdm(corpus)
  
  freqModel <<- createFreqModel(corpus, tdm)
  KNModel <<- createKNModel(corpus, tdm)
  
  "Done"
}

# Delete the objectst that were saved
deleteSavedObjects <- function(files = c("corpus", "tdm",
                                        "freqModel", "KNModel")){
  if(file.exists(trainFolder))
    folder <- trainFolder else
      folder <- originalDataFolder
  
  for(file in paste0("/", files, ".RData")){
    if(file.exists(paste0(folder, file))){
      print(paste("Deleting", paste0(folder, file)))
      file.remove(paste0(folder, file))
    }
  }
  
  "Done"
}