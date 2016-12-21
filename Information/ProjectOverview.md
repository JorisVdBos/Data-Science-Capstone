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
print(usePercentageOfData)
createSampleDataDir(usePercentageOfData, seed)
```

Reading the training sample data into a corpus is done using function "createCorpus". There was a great deal of filtering done in this function. The special characters and numbers were removed. The stopwords and conjugation words have deliberately been kept inside, so they can also be taken in account with the predictions. The most notable change I made was keeping all the "'" to preseve words such as "don't" and "I'm". Also, dots were replaced with a "\n" string. These denote the end and beginning of a senctence so they can also be processed in the prediction model. That way you can predict when a person will end the sentence, and make better preditions at the start of a sentence.

```r
corpus <- createCorpus()
tdm <- createTdm()
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
##  1:  the 14577
##  2:    , 13269
##  3:   to  9560
##  4:    a  7974
##  5:  and  7798
##  6:    i  7540
##  7:    #  7001
##  8:   of  6324
##  9:    !  5635
## 10:   in  4998
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
##  1:    1 16499 0.556721555
##  2:    2  4058 0.136928060
##  3:    3  2014 0.067957889
##  4:    4  1228 0.041436091
##  5:    5   858 0.028951275
##  6:    6   622 0.020987988
##  7:    7   461 0.015555406
##  8:    8   377 0.012721015
##  9:    9   305 0.010291537
## 10:   10   254 0.008570657
```

Creating the table of n-grams of length 2:

```r
n2grams <- ngramsFromCorpus(corpus, n = 2)
n2gramsTable <- data.table(get.phrasetable(n2grams))
n2gramsTable[, density := freq / sum(n2gramsTable$freq)]
n2gramsTable[order(-density)][1:10] # The 10 most frequent 2-grams
```

```
##      ngrams  freq        prop     density
##  1:  \n \n  33362 0.076822113 0.076822113
##  2:   ! \n   2875 0.006620214 0.006620214
##  3:   \n i   2695 0.006205731 0.006205731
##  4: of the   1258 0.002896775 0.002896775
##  5:   ? \n   1234 0.002841511 0.002841511
##  6: in the   1226 0.002823089 0.002823089
##  7: \n the   1161 0.002673415 0.002673415
##  8:  , and   1121 0.002581308 0.002581308
##  9:   \n #    934 0.002150706 0.002150706
## 10:   # \n    891 0.002051691 0.002051691
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
##            ngrams  freq         prop      density
##  1:     \n \n \n  16681 0.0384111450 0.0384111450
##  2:      ! \n \n   2846 0.0065534512 0.0065534512
##  3:      \n \n i   1426 0.0032836336 0.0032836336
##  4:      ? \n \n   1215 0.0027977664 0.0027977664
##  5:      \n \n #    694 0.0015980657 0.0015980657
##  6:    \n \n the    603 0.0013885211 0.0013885211
##  7:      # \n \n    557 0.0012825974 0.0012825974
##  8:        # u #    363 0.0008358759 0.0008358759
##  9: \n \n thanks    261 0.0006010017 0.0006010017
## 10:    \n \n you    234 0.0005388291 0.0005388291
```

A histogram of the freqency of the ngrams of length 3:

```r
ggplot(data= n3gramsTable) + 
  geom_freqpoly(aes(freq))
```

![](ProjectOverview_files/figure-html/ngram3hist-1.png)<!-- -->

# Modelling
Modelling functions are found in the folder "03_Modelling".

The bove code worked with 0.5% of the provided data. To be able to work with more data and not wait a long time for the corpus to be collected every time, I have saved the corpus using 5% of the data.

```r
load("TempData/corpus5perc.RData")
load("TempData/tdm5perc.RData")
```

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
# freqModel <- createFreqModel(corpus, tdm, loadingBar = FALSE)
load("TempData/freqModel5perc.RData")
print(predict(freqModel, "then you "))
```

```
##    value       source
## 1:   can n3gramsTable
## 2:  have n3gramsTable
## 3:  know n3gramsTable
```

```r
print(predict(freqModel, "sometimes you "))
```

```
##    value       source
## 1:  just n3gramsTable
## 2:  have n3gramsTable
## 3:   are n3gramsTable
```

```r
print(predict(freqModel, "xnjiqqfqsf you "))
```

```
##    value      source
## 1:   are 2gramsTable
## 2:   can 2gramsTable
## 3:  have 2gramsTable
```

```r
print(predict(freqModel, "xnjiqqfqsf nqnjndjiqpdns "))
```

```
##    value            source
## 1:   the wordFreqencyTable
## 2:    to wordFreqencyTable
## 3:   and wordFreqencyTable
```

To save space, the words were stored as an ordered data table. This way, they all have an unique index number, which can be used as quick lookup table for the words. The 2 and 3-grams are also ordered lists with the more prevalent combinations at the top. The downside of this model is that you cannot add new entries to it, because it cannot determine anymore if the new combinations are more prevalent than the allready existing ones in it. This can be fixed to add a new row to all tables with the freqencies but this was not part of the scope (for now).

```r
freqModel
```

```
## $wordFreqTable
##          word
##     1:    the
##     2:      ,
##     3:     to
##     4:    and
##     5:      a
##    ---       
## 48926:   zulu
## 48927:   zuzu
## 48928:     zx
## 48929: zygons
## 48930:     \n
## 
## $n2gramsTable
##         indexWord1 indexWord2
##      1:          0          0
##      2:          9          0
##      3:          0          6
##      4:          8          1
##      5:         10          1
##     ---                      
## 277700:       1049      28967
## 277701:       5231      48924
## 277702:          0      10684
## 277703:          0      35485
## 277704:       5806      21959
## 
## $n3gramsTable
##         indexWord1 indexWord2 indexWord3
##      1:          0          0          0
##      2:          9          9          0
##      3:          0          0          6
##      4:         18         18          0
##      5:          0          0          7
##     ---                                 
## 334776:        778          5       3884
## 334777:          1       4617       3884
## 334778:         13          1       3884
## 334779:          0          4      35483
## 334780:          0          0      10684
## 
## attr(,"class")
## [1] "list"      "FreqModel"
```

The size of the model:

```r
object.size(freqModel)
```

```
## 16504608 bytes
```

```r
object.size(freqModel) / 1024^2 # In megabytes
```

```
## 15.7400207519531 bytes
```

Evaluation of the model is done by this function. It will take the data from the testing map, made in the first chapter with the createSampleDataDir function. It will take the fraction you want (or everything) and check word for word if it could have been predicted with the model provided.

```r
test <- testModel(freqModel, fraction = 0.0005, loadingBar = FALSE)
```

```
## [1] "Total training lines for en_US.blogs.txt was 4496"
## [1] "Total testing lines for en_US.blogs.txt is 2"
## [1] "Total training lines for en_US.news.txt was 386"
## [1] "Total testing lines for en_US.news.txt is 0"
## [1] "Total training lines for en_US.twitter.txt was 11800"
## [1] "Total testing lines for en_US.twitter.txt is 5"
```

```r
test$score
```

```
##                   Words total Prediction success % successful prediction
## en_US.blogs.txt            95                 27               0.2842105
## en_US.news.txt              0                  0                     NaN
## en_US.twitter.txt          67                 12               0.1791045
## Total                     162                 39               0.2407407
```

```r
test$wordAccuracy
```

```
##       word1      word2      word3
## 1 0.1234568 0.06790123 0.04938272
```

So the prediction is about 17%, which is not very impressive.

The prediction succes can be higher by letting the prediction model give more options. If we change the model to give 5 possibilities instead of only 3, the succesrate goes up:

```r
giveNumberOfPossibilities <- 5
test <- testModel(freqModel, fraction = 0.0005, loadingBar = FALSE)
```

```
## [1] "Total training lines for en_US.blogs.txt was 4496"
## [1] "Total testing lines for en_US.blogs.txt is 2"
## [1] "Total training lines for en_US.news.txt was 386"
## [1] "Total testing lines for en_US.news.txt is 0"
## [1] "Total training lines for en_US.twitter.txt was 11800"
## [1] "Total testing lines for en_US.twitter.txt is 5"
```

```r
test$score
```

```
##                   Words total Prediction success % successful prediction
## en_US.blogs.txt            95                 29               0.3052632
## en_US.news.txt              0                  0                     NaN
## en_US.twitter.txt          67                 18               0.2686567
## Total                     162                 47               0.2901235
```

```r
test$wordAccuracy
```

```
##       word1      word2      word3      word4      word5
## 1 0.1234568 0.06790123 0.04938272 0.02469136 0.02469136
```

## Improving the model
The model above has gone through various steps of improving the model. These changes were:

* Adding sensitivity to numbers. Instead of dropping numbers altogether, they are converted to a single "#" character, which is - Adding sensitivity to "!" and "?"
* Dropping all n-grams with freqency 1 from the model makes it a lot smaller in size, and even improves the accuracy a little.
* Adding a filter to the predictions that allows for filtering to the first word (for example "I love y" will subset the prediction to all words starting with "y")

## Kneser-Ney model
I made a second model, following the formula's in [this paper](http://u.cs.biu.ac.il/~yogo/courses/mt2013/papers/chen-goodman-99.pdf).

```r
# KNModel <- createKNModel(corpus, tdm, loadingBar = FALSE)
load("TempData/KNModel5perc.RData")
```

```r
KNModel
```

```
## $wordFreqTable
##          word    freq index         prob
##     1:     \n 1232520     0 4.683839e-02
##     2:    the  145992     1 1.037530e-02
##     3:      ,  132833     2 3.458544e-02
##     4:     to   95823     3 1.362215e-02
##     5:    and   79166     4 2.264819e-02
##    ---                                  
## 48978: zulily       2 48977 3.323286e-06
## 48979:   zulu       2 48978 3.323286e-06
## 48980:   zuzu       2 48979 3.323286e-06
## 48981:     zx       2 48980 3.323286e-06
## 48982: zygons       2 48981 3.323286e-06
## 
## $n2gramsTable
##         indexWord1 indexWord2   freq
##      1:          0          0 333664
##      2:          9          0  28991
##      3:          0          6  26593
##      4:          8          1  12833
##      5:         10          1  12119
##     ---                             
## 278251:       1046      28991      2
## 278252:       5233      48976      2
## 278253:          0      10694      2
## 278254:          0      35505      2
## 278255:       5695      21976      2
## 
## $n3gramsTable
##         indexWord1 indexWord2 indexWord3   freq
##      1:          0          0          0 166836
##      2:          9          9          0  28709
##      3:          0          0          6  14217
##      4:         18         18          0  11885
##      5:          0          0          7   6580
##     ---                                        
## 335730:        778          5       3885      2
## 335731:          1       4620       3885      2
## 335732:         13          1       3885      2
## 335733:          0          4      35503      2
## 335734:          0          0      10694      2
## 
## attr(,"class")
## [1] "list"    "KNModel"
```

```r
giveNumberOfPossibilities <- 3
print(predict(freqModel, "then you "))
```

```
##    value       source
## 1:   can n3gramsTable
## 2:  have n3gramsTable
## 3:  know n3gramsTable
```

```r
print(predict(freqModel, "sometimes you "))
```

```
##    value       source
## 1:  just n3gramsTable
## 2:  have n3gramsTable
## 3:   are n3gramsTable
```

```r
print(predict(freqModel, "xnjiqqfqsf you "))
```

```
##    value      source
## 1:   are 2gramsTable
## 2:   can 2gramsTable
## 3:  have 2gramsTable
```

```r
print(predict(freqModel, "xnjiqqfqsf nqnjndjiqpdns "))
```

```
##    value            source
## 1:   the wordFreqencyTable
## 2:    to wordFreqencyTable
## 3:   and wordFreqencyTable
```

The size of the model:

```r
object.size(KNModel)
```

```
## 19060608 bytes
```

```r
object.size(KNModel) / 1024^2 # In megabytes
```

```
## 18.1776123046875 bytes
```

Testing the model:

```r
test <- testModel(KNModel, fraction = 0.0005, loadingBar = FALSE)
```

```
## [1] "Total training lines for en_US.blogs.txt was 4496"
## [1] "Total testing lines for en_US.blogs.txt is 2"
## [1] "Total training lines for en_US.news.txt was 386"
## [1] "Total testing lines for en_US.news.txt is 0"
## [1] "Total training lines for en_US.twitter.txt was 11800"
## [1] "Total testing lines for en_US.twitter.txt is 5"
```

```r
test$score
```

```
##                   Words total Prediction success % successful prediction
## en_US.blogs.txt            95                  0              0.00000000
## en_US.news.txt              0                  0                     NaN
## en_US.twitter.txt          67                  5              0.07462687
## Total                     162                  5              0.03086420
```

```r
test$wordAccuracy
```

```
##        word1      word2      word3
## 1 0.00617284 0.01234568 0.01234568
```


```r
giveNumberOfPossibilities <- 5
test <- testModel(KNModel, fraction = 0.0005, loadingBar = FALSE)
```

```
## [1] "Total training lines for en_US.blogs.txt was 4496"
## [1] "Total testing lines for en_US.blogs.txt is 2"
## [1] "Total training lines for en_US.news.txt was 386"
## [1] "Total testing lines for en_US.news.txt is 0"
## [1] "Total training lines for en_US.twitter.txt was 11800"
## [1] "Total testing lines for en_US.twitter.txt is 5"
```

```r
test$score
```

```
##                   Words total Prediction success % successful prediction
## en_US.blogs.txt            95                  2              0.02105263
## en_US.news.txt              0                  0                     NaN
## en_US.twitter.txt          67                  6              0.08955224
## Total                     162                  8              0.04938272
```

```r
test$wordAccuracy
```

```
##        word1      word2      word3      word4 word5
## 1 0.00617284 0.00617284 0.01234568 0.02469136     0
```

Though the predicted output is lower, it is said to better if the model is trained with more data. This will be worth testing out:
