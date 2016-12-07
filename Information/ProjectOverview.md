# Data science coursera track final project
Joris Van den Bossche  
26 september 2016  



# Introduction
Welcome reader to the documentation of my Coursera Data Science's final project. In this document I will provide you with an overview of my actions and ideas I had about the project.

You will see I have tried to divide all steps I have taken into functions. I will not go into detail about how they are constructed. For that you can delve into the R scripts themselves. Instead I will comment on what they do and show the output.

The supository can be found [here on github](https://github.com/JorisVdBos/Data-Science-Capstone).

# Global options
The libraries and all functions used are loaded in with the following file in the Global folder.

```r
source("00_Global/libraries.R")
```


# Reading in data
Functions are defined in the folder "01_Load" for downloading, sampling and loading the data into R.

## Usage
This function downloads the files from the internet, if they are not yet present.

```r
downloadFiles()
```

Because reading in all the data takes way too long, this function takes a fraction and puts it into a sample folder, sampleTrain. The leftover data is also copied, into two more folders: sampleTest and sampleValidate. These data sets are completely randomly chosen (hence the seed) and the maximum size is the amount of lines of the training fata.

```r
usePercentageOfData
createSampleDataDir(usePercentageOfData, seed)
```

Reading the training sample data into a corpus is done using function "createCorpus". There was a great deal of filtering done in this function. The special characters and numbers were removed. The stopwords and conjugation words have deliberately been kept inside, so they can also be taken in account with the predictions. The most notable change I made was keeping all the "'" to preseve words such as "don't" and "I'm". Also, dots were replaced with a "\n" string. These denote the end and beginning of a senctence so they can also be processed in the prediction model. That way you can predict when a person will end the sentence, and make better preditions at the start of a sentence.

```r
corpus <- createCorpus()
tdm <- TermDocumentMatrix(corpus, control = list(wordLengths=c(0, Inf)))
```

# Exploratory Analysis
These functions are found in the folder "02_ExploratoryAnalysis".

This function loads a random sample of the lines of data into R from the three sources.

```r
readTextsSample(lines = 2, seed = 104)
```

```
## $en_US.blogs.txt
## [1] "As a teenager, Eri develops the callingan internal link to the man-eating beasts plaguing the planet. She finds herself repeatedly drawn beyond the safety borders, driven by rage, hoping to satiate the bloodlust flooding her veins."
## [2] "Hearts starve as well as bodies:"                                                                                                                                                                                                        
## 
## $en_US.news.txt
## [1] "The test car never became tiresome in traffic, never jerked and bucked from too-little low-speed power as you engaged the clutch."                                                                                                                                                                                                                                       
## [2] "Kate might consider giving some advice to her younger sister on media management. While the duchess has generated some of the most positive royal press in years, Pippa Middleton made some unpleasant headlines this month when she was photographed in a car in Paris with a driver who pretended to point a gun at photographers. The gun was later said to be a toy."
## 
## $en_US.twitter.txt
## [1] "Damn my girl look good with a bowl full of chili"                     
## [2] "Watching #doomsdaypreppers...they make it seem so sane and logical..."
```

The amount of words and chars are counted in this function:

```r
probeData()
```

```
##                File   Lines    Words Characters Longest Line: Words
## 1   en_US.blogs.txt  899288 37334131  208361438                6630
## 2    en_US.news.txt   77259  2643969   15683765                1031
## 3 en_US.twitter.txt 2360148 30373543  162384825                  47
##   Longest Line: Characters
## 1                    40835
## 2                     5760
## 3                      213
```


Constructing a word freqency table from a term document matrix makes it possible to see the most frequent words. Since the input source of the words that eventually will be targeted to be predicted, is not specified, the words from all the sources are put together.

```r
wordFreqTable <- wordFreq(tdm)
wordFreqTable[1:10, ] # The 10 most frequent words
```

```
##     word  freq
##  1:  the 14554
##  2:    , 13346
##  3:   to  9549
##  4:    a  7942
##  5:  and  7784
##  6:    i  7538
##  7:   of  6306
##  8:   in  4985
##  9:  you  4152
## 10:   is  4046
```

Just for the fun of it, let's create a word cloud:

```r
wordcloud(words = corpus, scale=c(3, 1), max.words=150, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(7,"Greens"), random.color = TRUE)
```

![](ProjectOverview_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

Most words only appear once, which was somewhat to be expected. The frequency is distributed as such:

```r
density <- wordFreqTable[, .(count = .N), by = freq]
density[, density := count /sum(density$count)]
density[order(-density)][1:10] # The 10 most frequent word frequencies
```

```
##     freq count     density
##  1:    1 16624 0.558846270
##  2:    2  4048 0.136080949
##  3:    3  2018 0.067838774
##  4:    4  1203 0.040441053
##  5:    5   862 0.028977712
##  6:    6   626 0.021044139
##  7:    7   477 0.016035230
##  8:    8   362 0.012169294
##  9:    9   310 0.010421219
## 10:   10   251 0.008437826
```

Creating the table of n-grams of length 2:

```r
n2grams <- ngramsFromCorpus(corpus, n = 2)
n2gramsTable <- data.table(get.phrasetable(n2grams))
n2gramsTable[, density := freq / sum(n2gramsTable$freq)]
n2gramsTable[order(-density)][1:10] # The 10 most frequent 2-grams
```

```
##       ngrams freq        prop     density
##  1:    \n i  2683 0.006984542 0.006984542
##  2:  of the  1257 0.003272296 0.003272296
##  3:  in the  1220 0.003175975 0.003175975
##  4:  \n the  1162 0.003024986 0.003024986
##  5:   , and  1127 0.002933872 0.002933872
##  6:   , but   731 0.001902982 0.001902982
##  7:     , i   696 0.001811868 0.001811868
##  8: for the   693 0.001804058 0.001804058
##  9:  to the   692 0.001801455 0.001801455
## 10:  on the   608 0.001582781 0.001582781
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
##              ngrams freq         prop      density
##  1:  \n thanks for   162 0.0004217289 0.0004217289
##  2:      \n i have   146 0.0003800767 0.0003800767
##  3:      \n i love   131 0.0003410277 0.0003410277
##  4:        \n i am   128 0.0003332179 0.0003332179
##  5:   \n thank you   124 0.0003228049 0.0003228049
##  6: thanks for the   121 0.0003149951 0.0003149951
##  7:      \n if you   117 0.0003045820 0.0003045820
##  8:       \n i was   116 0.0003019787 0.0003019787
##  9:     \n i think   115 0.0002993755 0.0002993755
## 10:      \n it was   115 0.0002993755 0.0002993755
```

A histogram of the freqency of the ngrams of length 3:

```r
ggplot(data= n3gramsTable) + 
  geom_freqpoly(aes(freq))
```

![](ProjectOverview_files/figure-html/ngram3hist-1.png)<!-- -->

# Modelling
Modelling functions are found in the folder "03_Modelling".

## Model input
The model input is passed through the same filter as the corpus that was used to make the model. Here is an example of a conversion:

```r
removePunctuationsExeptions("His last album, \"The Great Escape\", has yet to see the light of day. \"I'm not even sure that album will come out under that title,\" he says. \"I do have the project completed and handed into E1\", his current record label. He says it could possibly hit in the fall.")
```

```
## [1] "his last album , the great escape , has yet to see the light of day \n i'm not even sure that album will come out under that title , he says \n i do have the project completed and handed into e # , his current record label \n he says it could possibly hit in the fall \n "
```

The model I made, will search the freqency table of the 3-grams and takes the three (by default) options that are most frequent. If less than three options are found, it will look at the 2-grams table. If still no options are found, it will look at the most freqent words. That way, the model will always have a guess, even though it has nothing to go for. I have also included the source of the prediction to be returned.

```r
freqModel <- createFreqModel(corpus, tdm, loadingBar = FALSE)
print(predict(freqModel, "then you "))
```

```
##    value       source
## 1: gotta n3gramsTable
## 2:   can  2gramsTable
## 3:  have  2gramsTable
```

```r
print(predict(freqModel, "sometimes you "))
```

```
##    value      source
## 1:   can 2gramsTable
## 2:  have 2gramsTable
## 3:   are 2gramsTable
```

```r
print(predict(freqModel, "xnjiqqfqsf you "))
```

```
##    value      source
## 1:   can 2gramsTable
## 2:  have 2gramsTable
## 3:   are 2gramsTable
```

```r
print(predict(freqModel, "xnjiqqfqsf nqnjndjiqpdns "))
```

```
##    value            source
## 1:   the wordFreqencyTable
## 2:    to wordFreqencyTable
## 3:     a wordFreqencyTable
```

To save space, the words were stored as an ordered data table. This way, they all have an unique index number, which can be used as quick lookup table for the words. The 2 and 3-grams are also ordered lists with the more prevalent combinations at the top. The downside of this model is that you cannot add new entries to it, because it cannot determine anymore if the new combinations are more prevalent than the allready existing ones in it. This can be fixed to add a new row to all tables with the freqencies but this was not part of the scope (for now).

```r
freqModel
```

```
## $wordFreqTable
##                 word
##     1:           the
##     2:             ,
##     3:            to
##     4:             a
##     5:           and
##    ---              
## 29744:    zuckerburg
## 29745:         zulfs
## 29746:        zusi's
## 29747: zzzzzzzzzzzzz
## 29748:            \n
## 
## $n2gramsTable
##        indexWord1 indexWord2
##     1:          0          6
##     2:          7          1
##     3:          8          1
##     4:          0          1
##     5:          2          5
##    ---                      
## 36547:          5       7057
## 36548:       1931       3527
## 36549:       1590       4366
## 36550:          1       9075
## 36551:          8      13123
## 
## $n3gramsTable
##        indexWord1 indexWord2 indexWord3
##     1:          0         85         11
##     2:          0          6         20
##     3:          0          6         68
##     4:          0          6         86
##     5:          0        170          9
##    ---                                 
## 24154:          7          1       1904
## 24155:         71        355       1905
## 24156:        400          2       5854
## 24157:          1       3506       3527
## 24158:        841       1590       4366
## 
## attr(,"class")
## [1] "list"      "FreqModel"
```

The size of the model:

```r
object.size(freqModel)
```

```
## 3089520 bytes
```

```r
object.size(freqModel) / 1024^2 # In megabytes
```

```
## 2.94639587402344 bytes
```

Evaluation of the model is done by this function. It will take the data from the testing map, made in the first chapter with the createSampleDataDir function. It will take the fraction you want (or everything) and check word for word if it could have been predicted with the model provided.

```r
test <- testModel(freqModel, fraction = 0.01, loadingBar = FALSE)
```

```
## [1] "Total training lines for en_US.blogs.txt was 4496"
## [1] "Total testing lines for en_US.blogs.txt is 44"
## [1] "Total training lines for en_US.news.txt was 386"
## [1] "Total testing lines for en_US.news.txt is 3"
## [1] "Total training lines for en_US.twitter.txt was 11800"
## [1] "Total testing lines for en_US.twitter.txt is 118"
```

```r
test$score
```

```
##                   Words total Prediction success % successful prediction
## en_US.blogs.txt          1652                343               0.2076271
## en_US.news.txt            163                 21               0.1288344
## en_US.twitter.txt        1485                286               0.1925926
## Total                    3300                650               0.1969697
```

```r
test$wordAccuracy
```

```
##       word1      word2      word3
## 1 0.1178788 0.04818182 0.03090909
```

So the prediction is about 17%, which is not very impressive.

The prediction succes can be higher by letting the prediction model give more options. If we change the model to give 5 possibilities instead of only 3, the succesrate goes up:

```r
giveNumberOfPossibilities <- 5
test <- testModel(freqModel, fraction = 0.005, loadingBar = FALSE)
```

```
## [1] "Total training lines for en_US.blogs.txt was 4496"
## [1] "Total testing lines for en_US.blogs.txt is 22"
## [1] "Total training lines for en_US.news.txt was 386"
## [1] "Total testing lines for en_US.news.txt is 1"
## [1] "Total training lines for en_US.twitter.txt was 11800"
## [1] "Total testing lines for en_US.twitter.txt is 59"
```

```r
test$score
```

```
##                   Words total Prediction success % successful prediction
## en_US.blogs.txt          1022                264               0.2583170
## en_US.news.txt             57                 12               0.2105263
## en_US.twitter.txt         824                184               0.2233010
## Total                    1903                460               0.2417236
```

```r
test$wordAccuracy
```

```
##      word1      word2      word3      word4      word5
## 1 0.112454 0.04308986 0.03205465 0.03415659 0.01996847
```

## Improving the model
The model above has gone through various steps of improving the model. These changes were:

* Adding sensitivity to numbers. Instead of dropping numbers altogether, they are converted to a single "#" character, which is - Adding sensitivity to "!" and "?"
* Dropping all n-grams with freqency 1 from the model makes it a lot smaller in size, and even improves the accuracy a little.
* Adding a filter to the predictions that allows for filtering to the first word (for example "I love y" will subset the prediction to all words starting with "y")
