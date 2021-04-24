
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

v_list <- list2[-5]

MrClean <- function(list){
  
  list_avgs <- lapply(list, function(x) lapply(x, mean, na.rm=TRUE))
  
  print(list_avgs)
  str(list_avgs)
  
  for (i in 1:length(list)) {
    
    print(i)
    
    list[[i]]$popularity <- replace_na(list[[i]]$popularity, list_avgs[[i]]$popularity)
    
    list[[i]]$danceability <- replace_na(list[[i]]$danceability, list_avgs[[i]]$danceability)
    
    list[[i]]$key <- replace_na(list[[i]]$key, list_avgs[[i]]$key)
    
    list[[i]]$energy <- replace_na(list[[i]]$energy, list_avgs[[i]]$energy)
    
    list[[i]]$loudness <- replace_na(list[[i]]$loudness, list_avgs[[i]]$loudness)
    
    list[[i]]$mode <- replace_na(list[[i]]$mode, list_avgs[[i]]$mode)
    
    list[[i]]$speechiness <- replace_na(list[[i]]$speechiness, list_avgs[[i]]$speechiness)
    
    list[[i]]$acousticness <- replace_na(list[[i]]$acousticness, list_avgs[[i]]$acousticness)
    
    list[[i]]$instrumentalness <- replace_na(list[[i]]$instrumentalness, list_avgs[[i]]$instrumentalness)
    
    list[[i]]$liveness  <- replace_na(list[[i]]$liveness , list_avgs[[i]]$liveness)
  
    list[[i]]$valence <- replace_na(list[[i]]$valence, list_avgs[[i]]$valence)
    
    list[[i]]$tempo <- replace_na(list[[i]]$tempo, list_avgs[[i]]$tempo)
  }
  
  return(list)
}

clean_list2 <- MrClean(v_list)


