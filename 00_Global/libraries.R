# Libraries
library(data.table)
library(ggplot2)
library(dplyr)
library(tm)
library(SnowballC)
library(Rgraphviz)
library(ngram)
library(RWeka)

# Source files
loadedAllScripts <- FALSE

for(folder in c("00_Global",
                "01_Load", 
                "02_ExploratoryAnalysis", 
                "03_Modelling")){
  
  for (script in list.files(folder)) {
    if(script != "libraries.R")
      source(file.path(folder, "/", script))
  }
}

loadedAllScripts <- TRUE