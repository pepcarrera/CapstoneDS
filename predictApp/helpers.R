library(stringr)

standardizeWord <- function(word) {
        word <- str_replace_all(word, "=|!|,|/.|-|_|#|?|!|'|:|*", "")
        word <- tolower(word)
        return(word)
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
