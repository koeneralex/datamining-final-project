library(shiny)

################################################################################
## Data -----

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

MrClean_Token <- function(list){
    
    token1 <- list %>% 
        select(lyrics) %>%
        unnest_tokens("word", "lyrics") %>%
        anti_join(get_stopwords())
    
}


#Hiphop - Setting up to combine playlists into genre
df1_w <- get_sentiment_data(clean_list[[1]])
df1_w$sentiment <- NULL
df1_w <- distinct(df1_w)
names(df1_w)[2] <- "freq"

df2_w <- get_sentiment_data(clean_list[[2]])
df2_w$sentiment <- NULL
df2_w <- distinct(df2_w)
names(df2_w)[2] <- "freq"

df3_w <- get_sentiment_data(clean_list[[3]])
df3_w$sentiment <- NULL
df3_w <- distinct(df3_w)
names(df3_w)[2] <- "freq"

#Pop - Setting up to combine playlists into genre
df4_w <- get_sentiment_data(clean_list[[4]])
df4_w$sentiment <- NULL
df4_w <- distinct(df4_w)
names(df4_w)[2] <- "freq"

df5_w <- get_sentiment_data(clean_list[[5]])
df5_w$sentiment <- NULL
df5_w <- distinct(df5_w)
names(df5_w)[2] <- "freq"


#Rock - Setting up to combine playlists into genre
df6_w <- get_sentiment_data(clean_list[[6]])
df6_w$sentiment <- NULL
df6_w <- distinct(df6_w)
names(df6_w)[2] <- "freq"

df7_w <- get_sentiment_data(clean_list[[7]])
df7_w$sentiment <- NULL
df7_w <- distinct(df7_w)
names(df7_w)[2] <- "freq"

df8_w <- get_sentiment_data(clean_list[[8]])
df8_w$sentiment <- NULL
df8_w <- distinct(df8_w)
names(df8_w)[2] <- "freq"


#Metal - Setting up to combine playlists into genre
df9_w <- get_sentiment_data(clean_list[[9]])
df9_w$sentiment <- NULL
df9_w <- distinct(df9_w)
names(df9_w)[2] <- "freq"

df10_w <- get_sentiment_data(clean_list[[10]])
df10_w$sentiment <- NULL
df10_w <- distinct(df10_w)
names(df10_w)[2] <- "freq"

df11_w <- get_sentiment_data(clean_list[[11]])
df11_w$sentiment <- NULL
df11_w <- distinct(df11_w)
names(df11_w)[2] <- "freq"


#Indie - Setting up to combine playlists into genre
df12_w <- get_sentiment_data(clean_list[[12]])
df12_w$sentiment <- NULL
df12_w <- distinct(df12_w)
names(df12_w)[2] <- "freq"

df13_w <- get_sentiment_data(clean_list[[13]])
df13_w$sentiment <- NULL
df13_w <- distinct(df13_w)
names(df13_w)[2] <- "freq"

df14_w <- get_sentiment_data(clean_list[[14]])
df14_w$sentiment <- NULL
df14_w <- distinct(df14_w)
names(df14_w)[2] <- "freq"


#Genres to choose from
Pop_s <- rbind(df4_w, df5_w)
Rock_s <- rbind(df6_w, df7_w, df8_w)
Hiphop_s <- rbind(df1_w, df2_w, df3_w)
Indie_s <- rbind(df12_w, df13_w, df14_w)
Metal_s <- rbind(df9_w, df10_w, df11_w)


#Bar plot function - emotions
bar_chart <- function(genre, title){
    
    playlist1_sentiment <- get_sentiment_data(genre)
    
    playlist1_sentiment_c <- subset(playlist1_sentiment, sentiment != "negative"& sentiment !="positive")
    
    ggplot(playlist1_sentiment_c, aes(x=sentiment, y=n, fill=sentiment)) +
        geom_col()+
        ggtitle(title)
}

#Bar plot function - positive vs negative
bar_chart_p <- function(genre, title){
    
    playlist1_sentiment <- get_sentiment_data(genre)
    
    playlist1_sentiment_c <- subset(playlist1_sentiment, sentiment == "negative"| sentiment =="positive")
    
    ggplot(playlist1_sentiment_c, aes(x=sentiment, y=n, fill=sentiment)) +
        geom_col()+
        ggtitle(title)
}

#Word cloud function
word_cloud <- function(genre){
    
    get_dat <- get_sentiment_data(genre)
    
    get_dat$sentiment <- NULL
    
    get_dat <- distinct(get_dat)
    
    names(get_dat)[2] <- "freq"
    
    cloud <- wordcloud2(get_dat)
    
    return(cloud)
}


word_cloud(Indie_s)
word_cloud(Hiphop_s)
word_cloud(Pop_s)
word_cloud(Metal_s)

bar_chart(Indie_s, "Indie")
bar_chart_p(Indie_s, "Indie")

bar_chart(Hiphop_s, "Hiphop")
bar_chart_p(Hiphop_s, "Hiphop")

bar_chart(Pop_s, "Pop")
bar_chart_p(Pop_s, "Pop")




################################################################################
## App -----

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Top Spotify Playlists - Wordcloud"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            selectInput(
                inputId = "genre_choice",
                label = "Choose Music Genre",
                choices = c("Pop_s", "Rock_s", "Hiphop_s", "Indie_s", "Metal_s"),
                selected = "Pop_s")
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
            
           plotOutput("plot")
           
         )
    
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$plot <- renderPlot({
       
        wordcloud2(input$genre_choice)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
