#### This is not as thorough because the API is no longer working ####
# This was used to clean January and February 2023



library("rdrop2")
library(tidyverse)
library(academictwitteR)
library(jsonlite)
library(data.table)
#install.packages("tidytext")
library(tidytext)

rm(list = ls())

clean_month <- function(x){
  #load("../confidential/twitter_dev_credentials.RData")
  
  tweet_files <- list.files("../downloaded_tweets/monthly_data")
  load(paste0("../downloaded_tweets/monthly_data/",tweet_files[x]))
  tweets_name <- paste0("tweets_", gsub(".RData", "", tweet_files[x]))
  tweets_name_clean <- paste0("clean_tweets_", gsub(".RData", "", tweet_files[x]))
  tweets <- get(tweets_name)
  
  
  tweets$text <- gsub("\\s?(f|ht)(tp)(s?)(://)([^\\.]*)[\\.|/](\\S*)", "", tweets$text)
  tweets$text <- gsub("@(\\S*)", "", tweets$text)
  
  tweets$wordcount <- sapply(strsplit(tweets$text, " "), length)
  
  tweets <- tweets[which(tweets$wordcount>6),]
  tweets <- tweets %>% 
    group_by(text) %>% 
    mutate(num_dups = n(), 
           dup_id = row_number()) %>% 
    ungroup() %>% 
    mutate(is_duplicated = dup_id > 1)
  
  # tweets <- tweets[which(tweets$dup_id==1),]
  # 
  # test <- get_user_profile(tweets$author_id,bearer_token)
  # #test1 <- test
  # 
  # test <-
  #   test[which(test$public_metrics$following_count/test$public_metrics$followers_count!="NaN"),]
  # test <-
  #   test[which(test$public_metrics$following_count/test$public_metrics$followers_count<100),]
  # test <-
  #   test[which(test$profile_image_url!="https://abs.twimg.com/sticky/default_profile_images/default_profile_normal.png"),]
  tweets <- rename(tweets, date = `date <- as.Date(created_at)`)
  # 
  # tweets <- tweets %>%
  #   filter(author_id %in% test$id)
  assign(eval(tweets_name_clean), tweets)
  # 
  # rm(test, tweets, tweet_files,
  #    bearer_token, consumer_key, consumer_secret)
  rm(list = ls()[ls()!=eval(tweets_name_clean)&ls()!="tweets_name_clean"])
  
  save(list = ls(), file = paste("../downloaded_tweets/cleaned_monthly_data/", tweets_name_clean, ".RData", sep = ""))
  rm(list = ls())
  
}

list.files("../downloaded_tweets/monthly_data")
length(list.files("../downloaded_tweets/monthly_data"))

# 61 and first batch of the day , check number
clean_month(66)

list.files("../downloaded_tweets/cleaned_monthly_data")
load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2023_02_February.RData")
load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2023_01_January.RData")

#lapply(6:24, clean_month)

#lapply(25:30, clean_month)

#load("../downloaded_tweets/cleaned_monthly_data/tweets_2017_09_September.RData")

# Şahinuç, F., & Toraman, C. (2021). Tweet Length Matters: A Comparative Analysis on Topic Detection in Microblogs. In D. Hiemstra, M.-F. Moens, J. Mothe, R. Perego, M. Potthast, & F. Sebastiani (Eds.), Advances in  Information Retrieval (pp. 471–478). Springer International Publishing. https://doi.org/10.1007/978-3-030-72240-1_50
