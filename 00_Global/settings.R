## In this file all the settings will be specified that will be used in the project

# Global settings
seed <- 10

# Reading data
lang <- "en_US"   # Language
usePercentageOfData <- 0.05

# Modelling
giveNumberOfPossibilities <- 3

doNotPredict <- c("#", "\n")





# Folders
##########################
rawDataFolder <- "RawData"
tempDataFolder <- "TempData"

originalDataFolder <- paste0(rawDataFolder, "/final/", lang)

trainFolder <- paste0(tempDataFolder, "/percentage", usePercentageOfData*100, "/sampleTrain")
testFolder <- paste0(tempDataFolder, "/percentage", usePercentageOfData*100, "/sampleTest")
validateFolder <- paste0(tempDataFolder, "/percentage", usePercentageOfData*100, "/sampleValidate")

# Filenames
dataFiles <- paste0(lang, c(".blogs.txt", ".news.txt", ".twitter.txt"))
