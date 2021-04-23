
#Code to remove and replace all NA in a list w/ group mean
#Victoria Garcia

library(dplyr)

list2_v <- list2

list2_v_avgs <- lapply(list2_v, function(x) lapply(x, mean, na.rm=TRUE))

  for (i in 1:length(list2_v)){
    
    print(i)
    
    list2_v[[i]]$popularity_2 <- replace_na(list2_v[[i]][["popularity"]], list2_v_avgs[[i]][["popularity"]])
    
    list2_v[[i]]$danceability_2 <- replace_na(list2_v[[i]][["danceability"]], list2_v_avgs[[i]][["danceability"]])
    
    list2_v[[i]]$key_2 <- replace_na(list2_v[[i]][["key"]], list2_v_avgs[[i]][["key"]])
    
    list2_v[[i]]$energy_2 <- replace_na(list2_v[[i]][["energy"]], list2_v_avgs[[i]][["energy"]])
    
    list2_v[[i]]$loudness_2 <- replace_na(list2_v[[i]][["loudness"]], list2_v_avgs[[i]][["loudness"]])
    
    list2_v[[i]]$mode_2 <- replace_na(list2_v[[i]][["mode"]], list2_v_avgs[[i]][["mode"]])
    
    list2_v[[i]]$speechiness_2 <- replace_na(list2_v[[i]][["speechiness"]], list2_v_avgs[[i]][["speechiness"]])
    
    list2_v[[i]]$acousticness_2 <- replace_na(list2_v[[i]][["acousticness"]], list2_v_avgs[[i]][["acousticness"]])
    
    list2_v[[i]]$instrumentalness_2 <- replace_na(list2_v[[i]][["instrumentalness"]], list2_v_avgs[[i]][["instrumentalness"]])
    
    list2_v[[i]]$valence_2 <- replace_na(list2_v[[i]][["valence"]], list2_v_avgs[[i]][["valence"]])
    
    list2_v[[i]]$liveness_2 <- replace_na(list2_v[[i]][["liveness"]], list2_v_avgs[[i]][["liveness"]])
    
    list2_v[[i]]$tempo_2 <- replace_na(list2_v[[i]][["tempo"]], list2_v_avgs[[i]][["tempo"]])
    }


