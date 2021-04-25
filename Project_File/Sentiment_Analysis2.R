library(spotifyr)
library(genius)
library(tm)
library(wordcloud)
library(XML)
library(tidyverse)
library(tidytext)
library(syuzhet)



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
