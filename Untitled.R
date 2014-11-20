file <- readLines(blogSampleFile,encoding="UTF-8")
x <- nchar(file)
y <- sort(x, decreasing=TRUE)
head(y)



```{r}
#directory <- DirSource("./en", "UTF-8", mode="text")
# dictCorpus <- enUsFiles
# stem words in a text document with Snowball
#library(SnowballC)
#library(RWeka)
#library(rJava)
#library(RWekajars)
#enUsFiles <- tm_map(enUsFiles, stemDocument, language="english")
#enUsFiles <- tm_map(enUsFiles, stemCompletion, dictionary=dictCorpus, mc.cores=1)
```


library(slam)
TDM.dense <- as.matrix(myTDM)
library(wordcloud)
library(RColorBrewer)
palette <- brewer.pal(9,"BuGn")[-(1:4)]
wordcloud(rownames(TDM.dense), rowSums(TDM.dense), min.freq = 1, color = palette)

library(reshape2)
TDM.dense = melt(TDM.dense, value.name = "count")
library(ggplot2)
ggplot(TDM.dense, aes(x = Docs, y = Terms, fill = log10(count))) +
        + geom_tile(colour = "white") +
        + scale_fill_gradient(high="#FF0000" , low="#FFFFFF")+
        + ylab("") +
        + theme(panel.background = element_blank()) +
        + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())



library(openNLP)
convert_text_to_sentences <- function(text, lang = "en") {
        # Function to compute sentence annotations using the Apache OpenNLP Maxent sentence detector employing the default model for language 'en'. 
        sentence_token_annotator <- Maxent_Sent_Token_Annotator(language = lang)
        
        # Convert text to class String from package NLP
        text <- as.String(text)
        
        # Sentence boundaries in text
        sentence.boundaries <- annotate(text, sentence_token_annotator)
        
        # Extract sentences
        sentences <- text[sentence.boundaries]
        
        # return sentences
        return(sentences)
}


# keep "r" by removing it from stopwords
# myStopwords <- c(stopwords('english'), "available", "via", "c")

decomp <- function(list) {
        x <- length(list)
        for (i in x) {
                output[i] <- list[i]
        }
        return output
}


quadgram <- NGramTokenizer(sample, Weka_control(min = 4, max = 4, delimeters = delimiter))
four_word <- data.frame(table(quadgram))
sort_four <- four_word[order(four_word$Freq,decreasing=TRUE),]
colnames(sort_four) <- c("Word", "Freq")
fourFreq <- sort_four[sort_four$Freq>10,]
fourFreq <- wordDf(fourFreq$Word)
colnames(fourFreq) <- c('w1', 'w2', 'w3', 'w4')


threeFreq[with(threeFreq, w1 == "must" & w2 == "be"),]


unigramTwitter <- NGramTokenizer(sample, Weka_control(min = 1, max = 1))
bigramTwitter <- NGramTokenizer(sample, Weka_control(min = 2, max = 2, delimeters = delimiter))
trigram <- NGramTokenizer(sample, Weka_control(min = 3, max = 3, delimeters = delimiter))



one_word <- data.frame(table(unigram))
two_word <- data.frame(table(bigram))
three_word <- data.frame(table(trigram))


sort_one <- one_word[order(one_word$Freq,decreasing=TRUE),]
colnames(sort_one) <- c("Word", "Freq")
sort_two <- two_word[order(two_word$Freq,decreasing=TRUE),]
colnames(sort_two) <- c("Word", "Freq")
sort_three <- three_word[order(three_word$Freq,decreasing=TRUE),]
colnames(sort_three) <- c("Word", "Freq")


oneFreq <- sort_one[sort_one$Freq>5,]
twoFreq <- sort_two[sort_two$Freq>5,]
threeFreq <- sort_three[sort_three$Freq>2,]