library(spotifyr)
library(genius)
library(tm)
library(wordcloud)
library(XML)
library(tidyverse)
library(tidytext)
library(syuzhet)
library(rtweet)         # For parsing the JSON
library(textclean)      # For removing contractions
library(stringr)        # For data cleaning 
library(plyr)          # For data manipulation
library(ggplot2)        # For plotting graphs
library(reshape2)       # For melting data
library(ggmap)          # For geospatial mapping
library(corrplot)
library(RColorBrewer) 
library(textdata)
library(dplyr)
library(stopwords)

df1 <- clean_list[[1]]
df2 <- clean_list[[2]]
df3 <- clean_list[[3]]
Hiphop <- rbind(df1, df2, df3)

token1 <- MrClean_Token(clean_list[[1]])
token1 <- MrClean_Token(Hiphop)

# Remove stop words
token1 <- token1 %>%
  anti_join(get_stopwords(), by = "word")

nrc_sent <- token1 %>%
  select(word) %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment) %>%
  arrange(desc(n))

nrc_polarity <- nrc_sent %>%
  filter(sentiment == "negative" | sentiment == "positive") %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup %>%
  mutate(word = reorder(word, n))

ggplot(nrc_polarity, aes(x = word, y = n, fill = sentiment)) +
  geom_col() +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Sentiments in One Hour of Gamestop Tweets",
       x = NULL,
       y = "Number of Words") +
  coord_flip()


ggplot(nrc_sent, aes(x=sentiment, y=n)) +
  geom_col()






get_sentiment_data <- function(list){
  
  token <- MrClean_Token(list)
  
  token <- token %>%
    anti_join(get_stopwords(), by = "word")
  
  nrc_sent <- token %>%
    select(word) %>%
    inner_join(get_sentiments("nrc")) %>%
    count(word, sentiment) %>%
    arrange(desc(n))
  
  return(nrc_sent)

}

playlist1_sentiment <- get_sentiment_data(clean_list[[1]])

ggplot(playlist1_sentiment, aes(x=sentiment, y=n)) +
  geom_col()





