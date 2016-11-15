## Useful functions:
#' findFreqTerms(tdm, 2, 3)
#' findAssocs(tdm, "oil", 0.7)
#' inspect(corpus)
#' meta(crude[[1]])
#' meta(crude[[1]], tag = "topics")
#' plot(tdm, corThreshold = 0.2, weighting = TRUE)
#' termFreq(crude[[14]])


source("01_Load/load.R")
if(!exists("corpus"))
  createCorpus()

# Create table with word freqencies
wordFreq <- function(tdm){
  capture.output(FreqMat <- data.frame(word = rownames(inspect(tdm)), freq = rowSums(inspect(tdm))), file='NUL')
  row.names(FreqMat) <- NULL
  FreqMat <-  data.table(FreqMat)
  FreqMat[order(-freq)]
}

# Create n-grams
ngramsFromCorpus <- function(corpus, n = 2) {
  allText <- paste(c(corpus[[1]]$content,
                     corpus[[2]]$content,
                     corpus[[3]]$content), collapse = " \n ")
  ngrams <- ngram(allText, n = n)
  ngrams
}

