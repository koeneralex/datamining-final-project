################################################################################
## Libraries

#install.packages("devtools")
#library(devtools)
#devtools::install_github("")
#install.packages('genius')
library(spotifyr)
library(genius)


################################################################################
# Spotify authorizations

# Authorize Spotify API info
Sys.setenv(SPOTIFY_CLIENT_ID = '3d49f13ca5124a8280e741a3f3842ff7')
Sys.setenv(SPOTIFY_CLIENT_SECRET = '6aaabaa303b548ee97f1b286a68933c9')
access_token <- get_spotify_access_token()


################################################################################
# Getting playlists

# Get playlist categories
cats <- get_categories()

# Get playlists for each category
hiphop <- get_category_playlists("hiphop")


################################################################################
## Efrain's playlist to df function

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

################################################################################



