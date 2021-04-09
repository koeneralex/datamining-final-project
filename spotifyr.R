# devtools::install_github('charlie86/spotifyr')
# install.packages('genius')
library(spotifyr)
library(genius)

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

# Add track name and song popularity to DF
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

test <- genius_lyrics(artist = df$V2[2], song = df$trackNames[2])
test_lyricCombo <- test$lyric[1:50,]
testDF$lyrics[2,] <- test_lyricCombo



