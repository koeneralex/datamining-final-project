#install.packages("devtools")
#library(devtools)
#devtools::install_github("")
#install.packages('genius')
library(spotifyr)
library(genius)



#############################################################
# ALEX K


# Authorize Spotify API info
Sys.setenv(SPOTIFY_CLIENT_ID = '3d49f13ca5124a8280e741a3f3842ff7')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '6aaabaa303b548ee97f1b286a68933c9')
access_token <- get_spotify_access_token()

# Get featured playlists on spotify
featuredPlaylists <- get_featured_playlists()

# Get playlist categories
cats <- get_categories()
hiphop <- get_category_playlists("hiphop")

# Extract RapCaviar playlist track names and clean data
RapCaviar <- get_playlist("37i9dQZF1DX0XUsuxWHRQd")


RapCaviarTracksList <- RapCaviar[["tracks"]]
RapCaviarTracksListItems <- RapCaviarTracksList[["items"]]


#RapCaviarTrackNamesList <-RapCaviar[["tracks"]][["items"]][["track.name"]]

# Add track name and song popularity to variables
trackNames <- RapCaviarTracksListItems$track.name
trackPopularity <- RapCaviarTracksListItems$track.popularity

# Song data to extract artist name
artistsSubdata <- RapCaviarTracksListItems$track.album.artists
artistsSubdata[[1]]$name

# Make df with track names
df <- data.frame(trackNames)

# Loop to extract artists names from each list at every level
for (i in 1:100){
  x <- artistsSubdata[[i]]$name
  df[i,c(2,3,4,5,6,7)] <- c(x)
}

# Remove the repeating values and other artists
df <- df[,-3:-7]

# Add track popularity to df
df$trackPopularity <- trackPopularity

# Using genius_lyrics to obtain song lyrics
lyrics <- genius_lyrics(df, df$V2, df$trackNames, type = "lyric")


# Testing genius package functions
testDF <- df

test <- genius_lyrics(artist = "ddg & og parker", song = "money long")
test_lyricCombo <- test$lyric[1:42,]
testDF$lyrics[2,] <- test_lyricCombo
#############################################################





#############################################################
# EFRAIN R.

#BLOCK OF TEXT - EFRAIN R. // DONT DELETE, GREAT TO REVIEW - ALEX K.
# #my test
# data1<- genius_lyrics(artist = df$V2[41], song = df$trackNames[41],info = "simple")
# 
# testData <- data1$lyric
# 
# string <- paste(unlist(testData),collapse=' ')
# 
# 
# functiondf <- data.frame(RapCaviar[["tracks"]][["items"]][["track.name"]],RapCaviar[["tracks"]][["items"]][["track.popularity"]])
# 
# colnames(functiondf)<-c("name","popularity")
# functiondf$name <-gsub("\\s*\\([^\\)]+\\)","",as.character(functiondf$name))
# 
# 
# vector1 <- character(nrow(functiondf))
# for(i in 1:nrow(functiondf)){
#   vector1[i]<- RapCaviar[["tracks"]][["items"]][["track.artists"]][[i]][["name"]][[1]]
# }
# functiondf$artist <- vector1
# 
# functiondf$lyrics <-NA
# 
# lyricsfromgenius <- genius_lyrics(artist= functiondf$artist[1],song = functiondf$name[1], info="simple")
# testData <- lyricsfromgenius$lyric
# string <- paste(unlist(testData),collapse=' ')
# functiondf$lyrics[1]<-string
# str(functiondf)

#function that creates a playlist to a data frame with name, artist, score, and lyrics
playlist_to_df <- function(playlist){
 
  #creates df of Track Names and Pop Score
  outputDF <- data.frame(playlist[["tracks"]][["items"]][["track.name"]],playlist[["tracks"]][["items"]][["track.popularity"]])
  colnames(outputDF)<-c("name","popularity")
  outputDF$name <-gsub("\\s*\\([^\\)]+\\)","",as.character(outputDF$name))
  #removes all parenthesis and everything inside them
  
  #artist name added to df
  vector1 <- character(nrow(outputDF))
  for(i in 1:nrow(outputDF)){
    vector1[i]<- playlist[["tracks"]][["items"]][["track.artists"]][[i]][["name"]][[1]]
  }
  outputDF$artist <- vector1
  
  #lyric time
  outputDF$lyrics <-NA
  
  #error handling to keep on adding to df if lyrics cannot be found
  for(i in 1:nrow(outputDF)){
    try({
      lyrics_from_genius <- genius_lyrics(artist = outputDF$artist[i],song = outputDF$name[i], info = "simple")
      lyric_list <- lyrics_from_genius$lyric
      outputDF$lyrics[i] <- paste(unlist(lyric_list),collapse=' ')
      
    })
  }
  return(outputDF)
}

#testing my function
#run lines 6-23, then function block, then you can run this test
#test took a while (1 min) to run on my computer. There will be error messages, so just wait til it completes
rc_df <- playlist_to_df(RapCaviar)
rc_df #10 missing song lyrics

#new playlist
MostNec <- get_playlist("37i9dQZF1DX2RxBh64BHjQ")
mn_df <- playlist_to_df(MostNec)
mn_df #22 missing song lyrics
#############################################################





##################-- Word Cloud Function --##################
## ALEX K

library(tm)
library(wordcloud)


## Tesing

# Words in vector
words_vec <- VectorSource(rc_df$lyrics[1])
head(words_vec)

# Words in corpus
words_corpus <- Corpus(words_vec)
head(words_corpus)

# Clean corpus
words_corpus <- tm_map(words_corpus, content_transformer(tolower))
words_corpus <- tm_map(words_corpus, removePunctuation)
words_corpus <- tm_map(words_corpus, removeNumbers)
words_corpus <- tm_map(words_corpus, removeWords, stopwords("english"))

# Make TDM and matrix to find word frequency
words_tdm <- TermDocumentMatrix(words_corpus)
words_m <- as.matrix(words_tdm)
wordCounts <- rowSums(words_m)
wordCounts <- sort(wordCounts, decreasing = T)
head(wordCounts)

# Plot wordcloud
wordcloud(names(wordCounts), wordCounts)




make_word_cloud <- function(lyrics) {
  
  # Words in vector
  words_vec <- VectorSource(lyrics)
  
  # Words in corpus
  words_corpus <- Corpus(words_vec)
  
  # Clean corpus
  words_corpus <- tm_map(words_corpus, content_transformer(tolower))
  words_corpus <- tm_map(words_corpus, removePunctuation)
  words_corpus <- tm_map(words_corpus, removeNumbers)
  words_corpus <- tm_map(words_corpus, removeWords, stopwords("english"))
  
  # Make TDM and matrix to find word frequency
  words_tdm <- TermDocumentMatrix(words_corpus)
  words_m <- as.matrix(words_tdm)
  wordCounts <- rowSums(words_m)
  wordCounts <- sort(wordCounts, decreasing = T)
  
  # Plot wordcloud
  wordCloud <- wordcloud(names(wordCounts), wordCounts)
  
  return(wordCloud)

}

# Testing function

make_word_cloud(rc_df$lyrics[1])

make_word_cloud(rc_df$lyrics[2])

make_word_cloud(rc_df$lyrics[3])
# Error when cleaning corpus

make_word_cloud(rc_df$lyrics[4])

#############################################################

