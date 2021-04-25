
#Code to remove and replace all NA in a list w/ group mean
#Victoria Garcia

library(dplyr)
library(spotifyr)
library(genius)
library(tm)
library(wordcloud)
library(tidyr)
library(lexicon)
library(tidytext)
library(ggplot2)
library(forcats)
library(hrbrthemes)
library(viridis)


MrClean <- function(list){
  
  list_avgs <- lapply(list, function(x) lapply(x, mean, na.rm=TRUE))
  
  print(list_avgs)
  str(list_avgs)
  
  for (i in 1:length(list)) {
    
    print(i)
    
    # Set the text to lowercase
    list[[i]]$lyrics <- tolower(list[[i]]$lyrics)
    
    # Remove mentions, urls, emojis, numbers, punctuations, etc.
    list[[i]]$lyrics <- gsub("@\\w+", "", list[[i]]$lyrics)
    list[[i]]$lyrics <- gsub("https?://.+", "", list[[i]]$lyrics)
    list[[i]]$lyrics <- gsub("\\d+\\w*\\d*", "", list[[i]]$lyrics)
    list[[i]]$lyrics <- gsub("#\\w+", "", list[[i]]$lyrics)
    list[[i]]$lyrics <- gsub("[^\x01-\x7F]", "", list[[i]]$lyrics)
    list[[i]]$lyrics <- gsub("[[:punct:]]", "", list[[i]]$lyrics)
    
    
    # Remove spaces and newlines
    list[[i]]$lyrics <- gsub("\n", " ", list[[i]]$lyrics)
    list[[i]]$lyrics <- gsub("^\\s+", "", list[[i]]$lyrics)
    list[[i]]$lyrics <- gsub("\\s+$", "", list[[i]]$lyrics)
    list[[i]]$lyrics <- gsub("[ |\t]+", " ", list[[i]]$lyrics)
    
    
    #Replace NA with mean
    list[[i]]$popularity <- replace_na(list[[i]]$popularity, as.integer(list_avgs[[i]]$popularity))
    list[[i]]$danceability <- replace_na(list[[i]]$danceability, list_avgs[[i]]$danceability)
    list[[i]]$key <- replace_na(list[[i]]$key, as.integer(list_avgs[[i]]$key))
    list[[i]]$energy <- replace_na(list[[i]]$energy, list_avgs[[i]]$energy)
    list[[i]]$loudness <- replace_na(list[[i]]$loudness, list_avgs[[i]]$loudness)
    list[[i]]$mode <- replace_na(list[[i]]$mode, as.integer(list_avgs[[i]]$mode))
    list[[i]]$speechiness <- replace_na(list[[i]]$speechiness, list_avgs[[i]]$speechiness)
    list[[i]]$acousticness <- replace_na(list[[i]]$acousticness, list_avgs[[i]]$acousticness)
    list[[i]]$instrumentalness <- replace_na(list[[i]]$instrumentalness, list_avgs[[i]]$instrumentalness)
    list[[i]]$liveness  <- replace_na(list[[i]]$liveness, list_avgs[[i]]$liveness)
    list[[i]]$valence <- replace_na(list[[i]]$valence, list_avgs[[i]]$valence)
    list[[i]]$tempo <- replace_na(list[[i]]$tempo, list_avgs[[i]]$tempo)
  }
  
  return(list)
  
}

clean_list <- MrClean(list4)

MrClean_Token <- function(list){
  
  token1 <- list %>% 
    select(lyrics) %>%
    unnest_tokens("word", "lyrics") %>%
    anti_join(get_stopwords())
  
}

token1 <- MrClean_Token(clean_list[[1]])





