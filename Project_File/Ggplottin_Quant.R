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



df1 <- clean_list[[1]]
df2 <- clean_list[[2]]
df3 <- clean_list[[3]]

Hiphop <- rbind(df1, df2, df3)

plot1 <- ggplot(Hiphop, aes(x=danceability, y=energy))+
  geom_point(aes(size=tempo, color=tempo))+
  guides(color= guide_legend(), size= guide_legend())+
  scale_color_gradient(
    low = "cyan",
    high = "darkblue"
  )+
  ggtitle("Hiphop")
plot1





