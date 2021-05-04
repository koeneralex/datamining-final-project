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
library(tidytext)


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
# WordCloud Spotify ------

# Tidy dictionaries

tidytext::sentiments

NRC <- nrc_emotions



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
testLyrics <- list1[[1]]$lyrics


words.vec <- VectorSource(testLyrics)
words.corpus <- Corpus(words.vec)
words.corpus

words.corpus <- tm_map(words.corpus, content_transformer(tolower))
words.corpus <- tm_map(words.corpus, removePunctuation)
words.corpus <- tm_map(words.corpus, removeNumbers)
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))
words.corpus <- tm_map(words.corpus, stripWhitespace)
words.corpus


tdm <- TermDocumentMatrix(words.corpus)
lyricsMatrix <- as.matrix(tdm)


wordCounts <- rowSums(lyricsMatrix)
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
## Syuzhet Package

#install.packages("syuzhet")
library(syuzhet)

testLyrics <- list1[[1]]$lyrics

df <- list4[[1]]
df$lyrics[1]

colnames(df)

# This is one song.
tokens <- df[1, ] %>% 
  unnest_tokens(input = "lyrics", 
                output = "text") %>%
  group_by(text) %>%
  tally(name = "freq") %>%
  arrange(desc(freq))


s <- get_nrc_sentiment(testLyrics)

barplot(colSums(s),
        las = 2,
        col = rainbow(10),
        ylab = 'count',
        main = 'Sentiment')

################################################################################
# Sentiment analysis for each playlist stored in "sentiment_analysis"

spotify_playlists_data <- clean_list

sentiment_analysis <- list()

for(i in 1:length(spotify_playlists_data)) {
  
  sentiment_analysis[[i]] <- get_nrc_sentiment(spotify_playlists_data[[i]]$lyrics)
  
}

View(sentiment_analysis)





get_sentiment_barplot <- function(sentiment_analysis_data) {
  
  barplot <- barplot(colSums(sentiment_analysis_data),
                     las = 2,
                     col = rainbow(10),
                     ylab = 'count',
                     main = 'Sentiment')
  return(barplot)
  
}
get_sentiment_barplot(sentiment_analysis[[1]])



################################################################################

sentiment_barplots <- list()

for(i in 1:length(sentiment_analysis)) {
  
  sentiment_barplots[[i]] <- get_sentiment_barplot(sentiment_analysis[[i]])
  
}

sentiment_barplots[[1]]
sentiment_barplots[[2]]
sentiment_barplots[[3]]

