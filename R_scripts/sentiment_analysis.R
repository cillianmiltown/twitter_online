#library(rtweet)
#install.packages("stopwords")
library(stopwords) 
library(dplyr) 
library(tidyr) 
library(tidytext) 
#install.packages("wordcloud", dependencies = TRUE)
library(wordcloud)
library(devtools)
library(tidyverse)      
library(stringr)
#install.packages("textdata")
library(textdata)
#install.packages("sentimentr")
library(sentimentr)
#install.packages("osfr")

rm(list = ls())


# x1 <- 2

list.files("../Data_for_ShinyApp/MeToo_tweets/clean")

month_data_fun <- function(x1){
  path <- "../Data_for_ShinyApp/MeToo_tweets/clean"
  
  
  
  month_data <- get(load(paste0(path,"/",list.files(path)[x1]))[1])
  
  df <- month_data
  
  df <- df[which(df$lang=="en"),]
  
  df[!grepl("Rajokri", df$text),]
  df <- df[!grepl("Rajokri", df$text),]
  
  df$text[grepl("video", df$text)]
  df <- df[!grepl("#HappyNewYear2023", df$text),]
  
  
  
  df <- df %>% 
    mutate(text = str_replace(text, "&amp;", " and "))
  df$text
  df <- df[which(df$dup_id==1),]
  
  
  
  
  
  tweet_sentences_data <- sentiment(get_sentences(df$text)) %>% 
    group_by(element_id) %>% 
    summarize(meanSentiment = mean(sentiment))
  head(tweet_sentences_data)
  df <- cbind.data.frame(df, tweet_sentences_data)
  
  
  df1 <- df %>% group_by(date) %>% 
    summarize(mean_daily_sentiment = mean(meanSentiment)
              , total_tweets = length(dup_id)
              , total_twwts_incl_retweets = sum(num_dups)
    )
  
  ## uncomment as required
  # for daily sentiment data
  # df1
  # for sentiment data for each tweet
  df
}


sentiment_data1 <- month_data_fun(2)
save(sentiment_data1, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data1.RData")
sentiment_data2 <- month_data_fun(3)
save(sentiment_data2, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data2.RData")
sentiment_data3 <- month_data_fun(4)
save(sentiment_data3, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data3.RData")
sentiment_data4 <- month_data_fun(5)
save(sentiment_data4, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data4.RData")
sentiment_data5 <- do.call(rbind, lapply(c(6:10), month_data_fun))
save(sentiment_data5, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data5.RData")
sentiment_data6 <- do.call(rbind, lapply(c(11:15), month_data_fun))
save(sentiment_data6, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data6.RData")
sentiment_data7 <- do.call(rbind, lapply(c(16:20), month_data_fun))
save(sentiment_data7, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data7.RData")
sentiment_data8 <- do.call(rbind, lapply(c(21:25), month_data_fun))
save(sentiment_data8, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data8.RData")
sentiment_data9 <- do.call(rbind, lapply(c(26:30), month_data_fun))
save(sentiment_data9, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data9.RData")
sentiment_data10 <- do.call(rbind, lapply(c(31:35), month_data_fun))
save(sentiment_data10, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data10.RData")
sentiment_data11 <- do.call(rbind, lapply(c(36:40), month_data_fun))
save(sentiment_data11, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data11.RData")
sentiment_data12 <- do.call(rbind, lapply(c(41:45), month_data_fun))
save(sentiment_data12, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data12.RData")
sentiment_data13 <- do.call(rbind, lapply(c(46:50), month_data_fun))
save(sentiment_data13, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data13.RData")
sentiment_data14 <- do.call(rbind, lapply(c(51:60), month_data_fun))
save(sentiment_data14, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data14.RData")
sentiment_data15 <- do.call(rbind, lapply(c(61:67), month_data_fun))
save(sentiment_data15, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data15.RData")


july_2020 <- month_data_fun(36)

x <- july_2020[which(july_2020$meanSentiment>0),]

x$text
df2 <- rbind.data.frame(
  #sentiment_data1
  sentiment_data2
  ,sentiment_data3
  ,sentiment_data4
  ,sentiment_data5
  ,sentiment_data6
  ,sentiment_data7
  ,sentiment_data8
  ,sentiment_data9
  ,sentiment_data10
  ,sentiment_data11
  ,sentiment_data12
  ,sentiment_data13
  ,sentiment_data14
  ,sentiment_data15
)


colnames(df2)
df2 <- `colnames<-`(df2, c("date","mean_daily_sentiment","total_tweets","total_tweets_incl_retweets" ))
sentiment_data_full <- df2
save(sentiment_data_full, file = "../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data_full.RData")


load("../Data_for_ShinyApp/MeToo_tweets/sentiment_sections/sentiment_data_full.RData")
df2 <- sentiment_data_full


ggplot(df2, aes(date, total_tweets, fill = mean_daily_sentiment)) + 
  scale_fill_continuous(low = "red",
                        high = "blue",)+
  geom_col()+
  theme_bw()



ggplot(df2, aes(date, mean_daily_sentiment, fill = total_tweets)) + 
  scale_fill_continuous(low = "grey",
                        high = "black",)+
  geom_col()+
  theme_bw()


