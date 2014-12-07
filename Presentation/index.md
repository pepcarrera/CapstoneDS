---
title       : Predicting Words
subtitle    : Based on twitter, news, and blog text data
author      : Pep Carrera
job         : Student in Coursera
framework   : io2012
highlighter : highlight.js
hitheme     : tomorrow      # 
widgets     : []            # {mathjax, quiz, bootstrap}
mode        : standalone # {standalone, draft}
knit        : slidify::knit2slides
---

## Introduction & The data

1. Our Application attemptes to predict the next word an will provide 3 possible next words
3. We use the data provided by the course, and we focus on the English data set
3. We then took a 5% probabilty random sample of twitter data, and 1% for news and blog data to make our corpus
4. Key delimeters were removed, but no words were stopped or removed to ensure good prediction

--- .class #id 

## The model

1. 1, 2, 3, and 4 gram data sets were created, and were sorted by frequency.
2. The n-gram data set was tabled, to get a frequency count
2. Each size n-gram was sorted by frequency, and any n-grams that did not have greater than 2 entries were dropped
3. For the unigram, only the top three words were kept
4. Below you will see the top 2 results of the quadgram



```r
head(fourFreq, 2)
```

```
##                           w1   w2  w3     w4
## thanks.for.the.follow thanks  for the follow
## the.rest.of.the          the rest  of    the
```

---

## Predicting Words

1. The prediction app breaks the phrase into words, and reuses the same word standardizing on the input phrase as was used to proceess the data set. 
2. Based on the words (up to a max of 3), we match against the frequency sorted n-gram predictor.  If it's 1 word, it uses the bigram, 2 words, the trigram, and 3 words the quadgram.  
3. The predition app returns the last word in the n-gram set as the predicted word.  It grabs the 3 highest matching n-gram sets for that set of words
4. If we get less than 3 predicted words, for each empty predicted word, we use a 'back off' approach that simply tries to predict on less input words.  Worst case, we grab the 3 most common words in the entire corpus (these were the top 3 words in the unigram)
5. Below if someone typed in a sentane ending with "thanks for the".  We get 1 hit on the quadgram, and 2 on the trigram, we would return:


```r
predictWord("thanks", "for", "the")
```

```
## [1] "follow"  "rt"      "mention"
```

---

## Using the App

1. Check out or app here: https://pepcarrera.shinyapps.io/predictionApp/PredictionApp.Rmd
2. In the "Type a sentance:" input box, just type in the sentance you want us to use to predict the next word on
3. The predictor will always use the last 3 words

