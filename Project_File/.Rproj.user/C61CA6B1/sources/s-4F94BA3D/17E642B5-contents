################################################################################
# Libraries --------

#install.packages("devtools")
#library(devtools)
#devtools::install_github("")
#install.packages('genius')
library(spotifyr)
library(genius)
library(tm)
library(wordcloud)
library(XML)
library(tidyverse)


################################################################################
# Sentiment Analysis MLK Example --------

pos <- "data/pos-words.txt"
neg <- "data/neg-words.txt"
p <- scan(pos, character(0), sep="\n")
n <- scan(neg, character(0), sep="\n")
head(p, 50)
head(n, 50)

p <- p[-1:-29]
n <- n[-1:-30]
head(p, 50)
head(n, 50)

mlkLocation <- URLencode("http://www.analytictech.com/mb021/mlk.htm")
doc.html = htmlTreeParse(mlkLocation, useInternal = TRUE)
mlk = unlist(xpathApply(doc.html, '//p', xmlValue))
mlk = gsub('\\n', '', mlk)
mlk = gsub('\\r', ' ', mlk)



words.vec <- VectorSource(mlk)
words.corpus <- Corpus(words.vec)
words.corpus

words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus

tdm <- TermDocumentMatrix(words.corpus)
mlkMatrix <- as.matrix(tdm)



wordCounts <- rowSums(mlkMatrix)
wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)

mlkWordcount <- length(wordCounts)


words <- names(wordCounts)
matchedpWords <- match(words, p, nomatch=0)
mpCounts <- matchedpWords[which(matchedpWords!=0)]
totalPos <- length(mpCounts)

totalPos

totalPos / mlkWordcount



wordsNeg <- names(wordCounts)
matchednWords <- match(wordsNeg, n, nomatch=0)
mnCounts <- matchednWords[which(matchednWords!=0)]
totalNeg <- length(mnCounts)

totalNeg

totalNeg / mlkWordcount



################################################################################
# Sentiment Analysis Spotify ------

# Standard positive/negative dictionary
pos <- "data/pos-words.txt"
neg <- "data/neg-words.txt"
p <- scan(pos, character(0), sep="\n")
n <- scan(neg, character(0), sep="\n")
head(p, 50)
head(n, 50)

p <- p[-1:-29]
n <- n[-1:-30]
head(p, 50)
head(n, 50)


# Add lyrics to a character vector here called "lyrics".


words.vec <- VectorSource(lyrics)
words.corpus <- Corpus(words.vec)
words.corpus

words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus


tdm <- TermDocumentMatrix(words.corpus)
lyricsMatrix <- as.matrix(tdm)


wordCounts <- rowSums(mlkMatrix)
wordCounts <- sort(wordCounts, decreasing=TRUE)
head(wordCounts)

lyricsWordcount <- length(wordCounts)


words <- names(wordCounts)
matchedPosWords <- match(words, p, nomatch=0)
matchedPosCounts <- matchedPosWords[which(matchedPosWords!=0)]
totalPos <- length(matchedPosCounts)

totalPos

totalPos / lyricsWordcount



wordsNeg <- names(wordCounts)
matchedNegWords <- match(wordsNeg, n, nomatch=0)
matchedNegCounts <- matchedNegWords[which(matchedNegWords!=0)]
totalNeg <- length(matchedNegCounts)

totalNeg

totalNeg / lyricsWordcount






################################################################################
