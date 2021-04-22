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
# Sentiment Analysis --------

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




# Step 5: Redo the positive and negative calculations for each 25% of the speech
# define a cutpoint to split the document into 4 parts; round the number to get an interger
cutpoint <- round(length(words.corpus)/4)

# First 25%
words.corpus1 <- words.corpus[1:cutpoint]
tdm1 <- TermDocumentMatrix(words.corpus1)
m1 <- as.matrix(tdm1)
wordCounts1 <- rowSums(m1)
wordCounts1 <- sort(wordCounts1, decreasing=TRUE)
length(wordCounts1)

# Positive
words1 <- names(wordCounts1)
matchedpWords1 <- match(words1, p, nomatch=0)
mpCounts1 <- matchedpWords1[which(matchedpWords1!=0)]
totalPos1 <- length(mpCounts1)
Q1P <- totalPos1 / length(wordCounts1)

# Negative
wordsNeg1 <- names(wordCounts1)
matchednWords1 <- match(wordsNeg1, n, nomatch=0)
mnCounts1 <- matchednWords1[which(matchednWords1!=0)]
totalNeg1 <- length(mnCounts1)
Q1N <- totalNeg1 / length(wordCounts1)


# Second 25%
words.corpus2 <- words.corpus[cutpoint+1:cutpoint*2]
tdm2 <- TermDocumentMatrix(words.corpus2)
m2 <- as.matrix(tdm2)
wordCounts2 <- rowSums(m2)
wordCounts2 <- sort(wordCounts2, decreasing=TRUE)
length(wordCounts2)

# Positive
words2 <- names(wordCounts2)
matchedpWords2 <- match(words2, p, nomatch=0)
mpCounts2 <- matchedpWords2[which(matchedpWords2!=0)]
totalPos2 <- length(mpCounts2)
Q2P <- totalPos2 / length(wordCounts2)

# Negative
wordsNeg2 <- names(wordCounts2)
matchednWords2 <- match(wordsNeg2, n, nomatch=0)
mnCounts2 <- matchednWords2[which(matchednWords2!=0)]
totalNeg2 <- length(mnCounts2)
Q2N <- totalNeg2 / length(wordCounts2)


# Third 25%
words.corpus3 <- words.corpus[cutpoint*2+1:cutpoint*3]
tdm3 <- TermDocumentMatrix(words.corpus3)
m3 <- as.matrix(tdm3)
wordCounts3 <- rowSums(m3)
wordCounts3 <- sort(wordCounts3, decreasing=TRUE)
length(wordCounts3)

# Positive
words3 <- names(wordCounts3)
matchedpWords3 <- match(words3, p, nomatch=0)
mpCounts3 <- matchedpWords3[which(matchedpWords3!=0)]
totalPos3 <- length(mpCounts3)
Q3P <- totalPos3 / length(wordCounts3)

# Negative
wordsNeg3 <- names(wordCounts3)
matchednWords3 <- match(wordsNeg3, n, nomatch=0)
mnCounts3 <- matchednWords3[which(matchednWords3!=0)]
totalNeg3 <- length(mnCounts3)
Q3N <- totalNeg3 / length(wordCounts3)


# Fourth 25%
words.corpus4 <- words.corpus[cutpoint*3+1:cutpoint*4]
tdm4 <- TermDocumentMatrix(words.corpus4)
m4 <- as.matrix(tdm4)
wordCounts4 <- rowSums(m4)
wordCounts4 <- sort(wordCounts4, decreasing=TRUE)
length(wordCounts4)

# Positive
words4 <- names(wordCounts4)
matchedpWords4 <- match(words4, p, nomatch=0)
mpCounts4 <- matchedpWords4[which(matchedpWords4!=0)]
totalPos4 <- length(mpCounts4)
Q4P <- totalPos4 / length(wordCounts4)

# Negative
wordsNeg4 <- names(wordCounts4)
matchednWords4 <- match(wordsNeg4, n, nomatch=0)
mnCounts4 <- matchednWords4[which(matchednWords4!=0)]
totalNeg4 <- length(mnCounts4)
Q4N <- totalNeg4 / length(wordCounts4)


# Graphing
qPos <- c(Q1P, Q2P, Q3P, Q4P)
qNeg <- c(Q1N, Q2N, Q3N, Q4N)
df <- data.frame(qPos, qNeg)
df$Qs <- c("Q1", "Q2", "Q3", "Q4")

# Positive Graphing
ggplot(df, aes(x=Qs, y=qPos)) +
  geom_col() +
  labs(title = "Positive Ratio")

# Negative Graphing
ggplot(df, aes(x=Qs, y=qNeg)) +
  geom_col() +
  labs(title = "Negative Ratio")




################################################################################