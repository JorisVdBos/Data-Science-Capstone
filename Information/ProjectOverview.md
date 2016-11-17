# Data science coursera track final project
Joris Van den Bossche  
26 september 2016  



# Introduction
Welcome reader to the documentation of my Coursera Data Science's final project. In this document I will provide you with an overview of my actions and ideas I had about the project.

You will see I have tried to divide all steps I have taken into functions. I will not go into detail about how they are constructed. For that you can delve into the R scripts themselves. Instead I will comment on what they do and show the output.

# Global options
global options and the libraries used are found in the Global folder.

```r
source("00_Global/libraries.R")
source("00_Global/settings.R")
```


# Reading in data
Functions are defined in this folder for downloading, sampling and loading the data into R.

```r
source("01_Load/load.R")
```

## Usage
This function downloads the files from the internet, if they are not yet present.

```r
downloadFiles()
```

Because reading in all the data takes way too long, this function takes a fraction and puts it into a sample folder, sampleTrain. The leftover data is also copied, into two more folders: sampleTest and sampleValidate. These data sets are completely randomly chosen (hence the seed) and the maximum size is the amount of lines of the training fata.

```r
usePercentageOfData
createSampleDataDir(usePercentageOfData, seed, lang = "en_US")
```

Reading the training sample data into a corpus is done using function "createCorpus". There was a great deal of filtering done in this function. The special characters and numbers were removed. The stopwords and conjugation words have deliberately been kept inside, so they can also be taken in account with the predictions. The most notable change I made was keeping all the "'" to preseve words such as "don't" and "I'm". Also, dots were replaced with a "\n" string. These denote the end and beginning of a senctence so they can also be processed in the prediction model. That way you can predict when a person will end the sentence, and make better preditions at the start of a sentence.

```r
corpus <- createCorpus(lang = "en_US")
tdm <- TermDocumentMatrix(corpus, control = list(wordLengths=c(0, Inf)))
```

# Exploratory Analysis
These functions are found in this folder:

```r
source("02_ExploratoryAnalysis/Explore.R")
```

This function loads a random sample of the lines of data into R from the three sources.

```r
readTextsSample(lines = 2, seed = 104)
```

```
## $en_US.blogs.txt
## [1] "As a teenager, Eri develops the callingan internal link to the man-eating beasts plaguing the planet. She finds herself repeatedly drawn beyond the safety borders, driven by rage, hoping to satiate the bloodlust flooding her veins."
## [2] "What if Ive been blue,"                                                                                                                                                                                                                 
## 
## $en_US.news.txt
## [1] "\"I think everybody pulled together, and we really fought hard,\" Quentin Richardson said. \"We were down, and J-Rich made two unbelievable shots. The whole night, we had to fight.\""
## [2] "Some markets, he said, like Texas, Northern California and Washington, D.C., have already reached that point."                                                                         
## 
## $en_US.twitter.txt
## [1] "No playoffs this year, another failed season. Where does this team go from here? They aren't close!"
## [2] "It's crazy how happy you make me. (:"
```

The amount of words and chars are counted in this function:

```r
probeData()
```

```
##                File Lines Words Characters Longest Line: Words
## 1   en_US.blogs.txt   899 38297     212479                 347
## 2    en_US.news.txt    77  2693      15831                 152
## 3 en_US.twitter.txt  2360 30566     163681                  34
##   Longest Line: Characters
## 1                     2004
## 2                      876
## 3                      265
```


Constructing a word freqency table from a term document matrix makes it possible to see the most frequent words. Since the input source of the words that eventually will be targeted to be predicted, is not specified, the words from all the sources are put together.

```r
wordFreqTable <- wordFreq(tdm)
wordFreqTable[1:10, ] # The 10 most frequent words
```

```
##     word freq
##  1:  the 2994
##  2:    , 2709
##  3:   to 1835
##  4:  and 1664
##  5:    i 1568
##  6:    a 1546
##  7:   of 1291
##  8:   in 1021
##  9:  you  822
## 10:   is  789
```

```r
wordFreqTable # The 10 most frequent words
```

```
##               word freq
##     1:         the 2994
##     2:           , 2709
##     3:          to 1835
##     4:         and 1664
##     5:           i 1568
##    ---                 
## 11529:        zopa    1
## 11530:       zorya    1
## 11531: zoryaborzoi    1
## 11532:    zucchini    1
## 11533:       zulfs    1
```

Most words only appear once, which was somewhat to be expected. The frequency is distributed as such:

```r
density <- wordFreqTable[, .(count = .N), by = freq]
density[, density := count /sum(density$count)]
density[order(-density)][1:10] # The 10 most frequent word frequencies
```

```
##     freq count     density
##  1:    1  6978 0.605046389
##  2:    2  1686 0.146189196
##  3:    3   766 0.066418105
##  4:    4   473 0.041012746
##  5:    5   262 0.022717420
##  6:    6   200 0.017341542
##  7:    7   131 0.011358710
##  8:    8   118 0.010231510
##  9:    9    94 0.008150525
## 10:   10    88 0.007630278
```

Creating the table of n-grams of length 2:

```r
n2grams <- ngramsFromCorpus(corpus, n = 2)
n2gramsTable <- data.table(get.phrasetable(n2grams))
n2gramsTable[, density := freq / sum(n2gramsTable$freq)]
n2gramsTable[order(-density)][1:10] # The 10 most frequent 2-grams
```

```
##      ngrams freq        prop     density
##  1:   \n i   543 0.006950934 0.006950934
##  2: in the   251 0.003213047 0.003213047
##  3: of the   242 0.003097838 0.003097838
##  4: \n the   233 0.002982629 0.002982629
##  5:  , and   232 0.002969828 0.002969828
##  6:  , but   155 0.001984152 0.001984152
##  7: to the   148 0.001894546 0.001894546
##  8:    , i   148 0.001894546 0.001894546
##  9: on the   143 0.001830541 0.001830541
## 10:  , the   115 0.001472113 0.001472113
```

A histogram of the freqency of the ngrams of length 2:

```r
ggplot(data= n2gramsTable) + 
  geom_freqpoly(aes(freq))
```

![](ProjectOverview_files/figure-html/ngram2hist-1.png)<!-- -->


As expected, most n-grams are quite rare. The same for the n-grams of length 3:

```r
n3grams <- ngramsFromCorpus(corpus, n = 3)
n3gramsTable <- data.table(get.phrasetable(n3grams))
n3gramsTable[, density := freq / sum(n3gramsTable$freq)]
n3gramsTable[order(-density)][1:10] # The 10 most frequent 3-grams
```

```
##             ngrams freq         prop      density
##  1:     \n i have    39 0.0004992447 0.0004992447
##  2:     \n i love    31 0.0003968356 0.0003968356
##  3:     \n if you    30 0.0003840344 0.0003840344
##  4:      \n i was    25 0.0003200287 0.0003200287
##  5:      a lot of    25 0.0003200287 0.0003200287
##  6:  \n thank you    24 0.0003072275 0.0003072275
##  7:    \n this is    24 0.0003072275 0.0003072275
##  8: \n thanks for    23 0.0002944264 0.0002944264
##  9:       , but i    23 0.0002944264 0.0002944264
## 10:       \n i am    22 0.0002816252 0.0002816252
```

A histogram of the freqency of the ngrams of length 3:

```r
ggplot(data= n3gramsTable) + 
  geom_freqpoly(aes(freq))
```

![](ProjectOverview_files/figure-html/ngram3hist-1.png)<!-- -->

# Modelling
Modelling functions are found in this folder:

```r
source("03_Modelling/Models.R")
```

This first model I made, will search the freqency table of the 3-grams and takes the three (by default) options that are most frequent. If less than three options are found, it will look at the 2-grams table. If still no options are found, it will look at the most freqent words. That way, the model will always have a guess, even though it has nothing to go for. I have also included the source of the prediction to be returned.

```r
freqModel <- createFreqModel(corpus, tdm)
print(predict(freqModel, "then", "you"))
```

```
##    value       source
## 1: would n3gramsTable
## 2:  meet n3gramsTable
## 3:    \n  2gramsTable
```

```r
print(predict(freqModel, "sometimes", "you"))
```

```
##    value       source
## 1:  make n3gramsTable
## 2:   try n3gramsTable
## 3:    \n  2gramsTable
```

```r
print(predict(freqModel, "xnjiqqfqsf", "you"))
```

```
##    value      source
## 1:    \n 2gramsTable
## 2:   can 2gramsTable
## 3:   are 2gramsTable
```

```r
print(predict(freqModel, "xnjiqqfqsf", "nqnjndjiqpdns"))
```

```
##    value            source
## 1:   the wordFreqencyTable
## 2:     , wordFreqencyTable
## 3:    to wordFreqencyTable
```

To save space, the words were stored as an ordered data table. This way, they all have an unique index number, which can be used as quick lookup table for the words. The 2 and 3-grams are also ordered lists with the more prevalent combinations at the top. The downside of this model is that you cannot add new entries to it, because it cannot determine anymore if the new combinations are more prevalent than the allready existing ones in it. This can be fixed to add a new row to all tables with the freqencies but this was not part of the scope (for now).

```r
freqModel
```

```
## $wordFreqTable
##               word
##     1:         the
##     2:           ,
##     3:          to
##     4:         and
##     5:           i
##    ---            
## 11530:       zorya
## 11531: zoryaborzoi
## 11532:    zucchini
## 11533:       zulfs
## 11534:          \n
## 
## $n2gramsTable
##        indexWord1 indexWord2
##     1:          0          5
##     2:          8          1
##     3:          7          1
##     4:          0          1
##     5:          2          4
##    ---                      
## 48492:       7954      11529
## 48493:       1285      11530
## 48494:          0      11531
## 48495:          1      11532
## 48496:        200      11533
## 
## $n3gramsTable
##        indexWord1 indexWord2 indexWord3
##     1:          0          5         20
##     2:          0          5         66
##     3:          0         34          9
##     4:          6        197          7
##     5:          0          5         17
##    ---                                 
## 72149:          7       7954      11529
## 72150:          1       1285      11530
## 72151:       1552          0      11531
## 72152:          7          1      11532
## 72153:          2        200      11533
## 
## attr(,"class")
## [1] "list"      "FreqModel"
```

The size of the model:

```r
object.size(freqModel)
```

```
## 3387008 bytes
```

```r
object.size(freqModel) / 1024^2 # In megabytes
```

```
## 3.2301025390625 bytes
```

Evaluation of the model is done by this function. It will take the data from the testing map, made in the first chapter with the createSampleDataDir function. It will take the fraction you want (or everything) and check word for word if it could have been predicted with the model provided.

```r
source("03_Modelling/Evaluation.R")
testModel(freqModel, fraction = 0.1)
```

```
## [1] "Total training lines for en_US.blogs.txt was 899"
## [1] "Total testing lines for en_US.blogs.txt is 89"
## [1] "Total training lines for en_US.news.txt was 77"
## [1] "Total testing lines for en_US.news.txt is 7"
## [1] "Total training lines for en_US.twitter.txt was 2360"
## [1] "Total testing lines for en_US.twitter.txt is 236"
```

```
##                   Words attempted to be predicted Prediction was a success
## en_US.blogs.txt                              4028                      748
## en_US.news.txt                                388                       66
## en_US.twitter.txt                            3227                      502
## Total                                        7643                     1316
##                   Percentage successful preduction
## en_US.blogs.txt                          0.1857001
## en_US.news.txt                           0.1701031
## en_US.twitter.txt                        0.1555624
## Total                                    0.1721837
```

So the prediction is about 17%, which is not very impressive.

The prediction succes can be higher by letting the prediction model give more options. If we change the model to give 5 possibilities instead of only 3, the succesrate goes up:

```r
source("03_Modelling/Evaluation.R")
giveNumberOfPossibilities <- 5
testModel(freqModel, fraction = 0.1)
```

```
## [1] "Total training lines for en_US.blogs.txt was 899"
## [1] "Total testing lines for en_US.blogs.txt is 89"
## [1] "Total training lines for en_US.news.txt was 77"
## [1] "Total testing lines for en_US.news.txt is 7"
## [1] "Total training lines for en_US.twitter.txt was 2360"
## [1] "Total testing lines for en_US.twitter.txt is 236"
```

```
##                   Words attempted to be predicted Prediction was a success
## en_US.blogs.txt                              4028                      982
## en_US.news.txt                                388                       84
## en_US.twitter.txt                            3227                      635
## Total                                        7643                     1701
##                   Percentage successful preduction
## en_US.blogs.txt                          0.2437934
## en_US.news.txt                           0.2164948
## en_US.twitter.txt                        0.1967772
## Total                                    0.2225566
```

For the next model I will have to check where the model fails and what can be done to improve it!
