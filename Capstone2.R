setwd("/Users/pcarrer1/Git/DSCapstone")

library(SnowballC)
library(RWeka)
library(rJava)
library(RWekajars)
library(tm)
library(stringr)

twitterFile <- "./en/en_US.twitter.sam.txt"
twitterSampleFile <- "./final/en_US/en_US.twitter.txt"
blogsFile1 <- "./en/en_US.blogs1.sam.txt"
blogsFile2 <- "./en/en_US.blogs1.sam.txt"
blogsSampleFile <- "./final/en_US/en_US.blogs.txt"
newsFile1 <- "./en/en_US.news1.sam.txt"
newsFile2 <- "./en/en_US.news2.sam.txt"
newsSampleFile <- "./final/en_US/en_US.news.txt"
delimiter <- " \\t\\r\\n.!?,;\"()"

wordDf <- function(inList) {
        outputDf <- toString(inList)
        outputDf <- unlist(strsplit(outputDf, ", "))
        outputDf <- as.data.frame(t(data.frame(sapply(outputDf, strsplit, split=' '))))
        return(outputDf)
}
standardizeWord <- function(word) {
        word <- str_replace_all(word, "=|!|,|/.|-|_|#|?|!|'|:|*", "")
        word <- tolower(word)
        return(word)
}

sampleFile <- function(infile,outfile, prob=.05) {
        ci <- file(infile,"r")
        co <- file(outfile,"w")
        recnum <- 0
        numout <- 0
        while (TRUE) {
                inrec <- readLines(ci,n=1, encoding="UTF-8", skipNul=TRUE)
                if (length(inrec) == 0) { # end of file?
                        close(co) 
                        return(numout)
                }
                recnum <- recnum + 1
                if (rbinom(1,1,p=prob) == 1) {
                        numout <- numout + 1
                        # remove special characters
                        inrec <- str_replace_all(inrec, "=|!|,|/.|-|_|#|?|!|'|:|*", "")
                        inrec <- str_replace_all(inrec, "'", "\'")
                        # move to lower case
                        inrec <- tolower(inrec)
                        # remove numbers
                        inrec <- removeNumbers(inrec)
                        #strip extra spaces
                        inrec <- tm::stripWhitespace(inrec)
                        writeLines(inrec,co)
                }
        }
        close(co)
}

predictWord <- function(word1, word2="", word3="") {
        if (word3 == "" & word2 == "") {
                word1 <- standardizeWord(word1)
                predDf2 <- twoFreq[with(twoFreq, w1 == word1),]
                predWord1 <- toString(unlist(predDf2[1,2]))
                predWord2 <- toString(unlist(predDf2[2,2]))
                predWord3 <- toString(unlist(predDf2[3,2]))
        } else if (word3 == "") {
                word1 <- standardizeWord(word1)
                word2 <- standardizeWord(word2)
                predDf2 <- twoFreq[with(twoFreq, w1 == word2),]
                predDf3 <- threeFreq[with(threeFreq, w1 == word1 & w2 == word2),]
                predWord1 <- toString(unlist(predDf3[1,3]))
                predWord2 <- toString(unlist(predDf3[2,3]))
                predWord3 <- toString(unlist(predDf3[3,3]))
                if (predWord1 == "NA") {
                        predWord1 <- toString(unlist(predDf2[1,2]))
                        predWord2 <- toString(unlist(predDf2[2,2]))
                        predWord3 <- toString(unlist(predDf2[3,2]))
                } else if (predWord2 == "NA" & predWord3 == "NA") {
                        predWord2 <- toString(unlist(predDf2[2,2]))
                        predWord3 <- toString(unlist(predDf2[3,2]))  
                } else if (predWord3 == "NA") {
                        predWord3 <- toString(unlist(predDf2[3,2]))
                }
        } else {
                word1 <- standardizeWord(word1)
                word2 <- standardizeWord(word2)
                word3 <- standardizeWord(word3)
                predDf2 <- twoFreq[with(twoFreq, w1 == word3),]
                predDf3 <- threeFreq[with(threeFreq, w1 == word2 & w2 == word3),]
                predDf4 <- fourFreq[with(fourFreq, w1 == word1 & w2 == word2 & w3 ==word3),]
                predWord1 <- toString(unlist(predDf4[1,4]))
                predWord2 <- toString(unlist(predDf4[2,4]))
                predWord3 <- toString(unlist(predDf4[3,4]))
                if (predWord1 == "NA") {
                        predWord1 <- toString(unlist(predDf3[1,3]))
                        predWord2 <- toString(unlist(predDf3[2,3]))
                        predWord3 <- toString(unlist(predDf3[3,3]))
                } else if (predWord2 == "NA" & predWord3 == "NA") {
                        predWord2 <- toString(unlist(predDf3[2,3]))
                        predWord3 <- toString(unlist(predDf3[3,3]))  
                } else if (predWord3 == "NA") {
                        predWord3 <- toString(unlist(predDf3[3,3]))
                }

                if (predWord1 == "NA") {
                        predWord1 <- toString(unlist(predDf2[1,2]))
                        predWord2 <- toString(unlist(predDf2[2,2]))
                        predWord3 <- toString(unlist(predDf2[3,2]))
                } else if (predWord2 == "NA" & predWord3 == "NA") {
                        predWord2 <- toString(unlist(predDf2[2,2]))
                        predWord3 <- toString(unlist(predDf2[3,2]))  
                } else if (predWord3 == "NA") {
                        predWord3 <- toString(unlist(predDf2[3,2]))
                }
        }

        checkWord(predWord1, predWord2, predWord3)
}

checkWord <- function(predWord1, predWord2, predWord3) {
        if (predWord1 == "NA") predWord1 <- noWord[1]
        if (predWord2 == "NA") predWord2 <- noWord[2]
        if (predWord3 == "NA") predWord3 <- noWord[3]
        predWords <- c(predWord1, predWord2, predWord3)
        return(predWords)
}

sampleFile(twitterSampleFile, twitterFile)
sampleFile(blogsSampleFile, blogsFile1, prob=.01)
sampleFile(blogsSampleFile, blogsFile2, prob=.01)
sampleFile(newsSampleFile, newsFile1, prob=.01)
sampleFile(newsSampleFile, newsFile2, prob=.01)

twitter <- readLines(twitterFile)
blogs1 <- readLines(blogsFile1)
blogs2 <- readLines(blogsFile2)
news1 <- readLines(newsFile1)
news2 <- readLines(newsFile2)

unigramTwitter <- NGramTokenizer(twitter, Weka_control(min = 1, max = 1))
bigramTwitter <- NGramTokenizer(twitter, Weka_control(min = 2, max = 2, delimeters = delimiter))
trigramTwitter <- NGramTokenizer(twitter, Weka_control(min = 3, max = 3, delimeters = delimiter))
quadgramTwitter <- NGramTokenizer(twitter, Weka_control(min = 4, max = 4, delimeters = delimiter))

unigramBlogs1 <- NGramTokenizer(blogs1, Weka_control(min = 1, max = 1))
bigramBlogs1 <- NGramTokenizer(blogs1, Weka_control(min = 2, max = 2, delimeters = delimiter))
trigramBlogs1 <- NGramTokenizer(blogs1, Weka_control(min = 3, max = 3, delimeters = delimiter))
quadgramBlogs1 <- NGramTokenizer(blogs1, Weka_control(min = 4, max = 4, delimeters = delimiter))

unigramBlogs2 <- NGramTokenizer(blogs2, Weka_control(min = 1, max = 1))
bigramBlogs2 <- NGramTokenizer(blogs2, Weka_control(min = 2, max = 2, delimeters = delimiter))
trigramBlogs2 <- NGramTokenizer(blogs2, Weka_control(min = 3, max = 3, delimeters = delimiter))
quadgramBlogs2 <- NGramTokenizer(blogs2, Weka_control(min = 4, max = 4, delimeters = delimiter))

unigramNews1 <- NGramTokenizer(news1, Weka_control(min = 1, max = 1))
bigramNews1 <- NGramTokenizer(news1, Weka_control(min = 2, max = 2, delimeters = delimiter))
trigramNews1 <- NGramTokenizer(news1, Weka_control(min = 3, max = 3, delimeters = delimiter))
quadgramNews1 <- NGramTokenizer(news1, Weka_control(min = 4, max = 4, delimeters = delimiter))

unigramNews2 <- NGramTokenizer(news2, Weka_control(min = 1, max = 1))
bigramNews2 <- NGramTokenizer(news2, Weka_control(min = 2, max = 2, delimeters = delimiter))
trigramNews2 <- NGramTokenizer(news2, Weka_control(min = 3, max = 3, delimeters = delimiter))
quadgramNews2 <- NGramTokenizer(news2, Weka_control(min = 4, max = 4, delimeters = delimiter))

unigram <- c(unigramTwitter, unigramBlogs1, unigramBlogs2, unigramNews1, unigramNews2)
bigram <- c(bigramTwitter, bigramBlogs1, bigramBlogs2, bigramNews1, bigramNews2)
trigram <- c(trigramTwitter, trigramBlogs1, trigramBlogs2, trigramNews1, trigramNews2)
quadgram <- c(quadgramTwitter, quadgramBlogs1, quadgramBlogs2, quadgramNews1, quadgramNews2)

one_word <- data.frame(table(unigram))
two_word <- data.frame(table(bigram))
three_word <- data.frame(table(trigram))
four_word <- data.frame(table(quadgram))


sort_one <- one_word[order(one_word$Freq,decreasing=TRUE),]
colnames(sort_one) <- c("Word", "Freq")
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
colnames(sort_two) <- c("Word", "Freq")
sort_three <- three_word[order(three_word$Freq,decreasing=TRUE),]
colnames(sort_three) <- c("Word", "Freq")
sort_four <- four_word[order(four_word$Freq,decreasing=TRUE),]
colnames(sort_four) <- c("Word", "Freq")



oneFreq <- sort_one[sort_one$Freq>2,]
twoFreq <- sort_two[sort_two$Freq>2,]
threeFreq <- sort_three[sort_three$Freq>2,]
fourFreq <- sort_four[sort_four$Freq>2,]

oneFreq <- wordDf(oneFreq$Word)
colnames(oneFreq) <- 'w1'
twoFreq <- wordDf(twoFreq$Word)
colnames(twoFreq) <- c('w1', 'w2')
threeFreq <- wordDf(threeFreq$Word)
colnames(threeFreq) <- c('w1', 'w2', 'w3')
fourFreq <- wordDf(fourFreq$Word)
colnames(fourFreq) <- c('w1', 'w2', 'w3', 'w4')


oneFreq <- oneFreq[!with(oneFreq, w1 == "fuck" | w1 == "dick" | w1 == "shit" | w1 == "piss" | w1 == "cunt" | w1 == "cocksucker" | w1 == "motherfucker" | w1 == "tits"),]
twoFreq <- twoFreq[!with(twoFreq, w2 == "fuck" | w2 == "dick" | w2 == "shit" | w2 == "piss" | w2 == "cunt" | w2 == "cocksucker" | w2 == "motherfucker" | w2 == "tits"),]
threeFreq <- threeFreq[!with(threeFreq, w3 == "fuck" | w3 == "dick" | w3 == "shit" | w3 == "piss" | w3 == "cunt" | w3 == "cocksucker" | w3 == "motherfucker" | w3 == "tits"),]
fourFreq <- fourFreq[!with(fourFreq, w4 == "fuck" | w4 == "dick" | w4 == "shit" | w4 == "piss" | w4 == "cunt" | w4 == "cocksucker" | w4 == "motherfucker" | w4 == "tits"),]

noWord <- character()
noWord[1] <- toString(unlist(oneFreq[1]))
noWord[2] <- toString(unlist(oneFreq[2]))
noWord[3] <- toString(unlist(oneFreq[3]))

