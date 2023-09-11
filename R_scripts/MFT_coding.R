rm(list = ls())
#install.packages("devtools")
#devtools::install_github("kbenoit/quanteda.dictionaries")
library("quanteda")
library("quanteda.dictionaries")
library(sentimentr)
library(dplyr)
library(stringr)

# path <- "../Data_for_ShinyApp/MeToo_tweets/clean"
# list.files(path)

month_MFT_fun <- function(x1){
  path <- "../Data_for_ShinyApp/MeToo_tweets/clean"
  tweet_files <- list.files(path)
  
  month_data <- get(load(paste0(path,"/",list.files(path)[x1]))[1])
  tweets_name_clean <- paste0("MFT_", gsub(".RData", "", tweet_files[x1]))
  
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
  
  
  
  output_lsd <- liwcalike(df$text, dictionary = data_dictionary_MFD)
  
  
  tweet_sentences_data <- sentiment(get_sentences(df$text)) %>% 
    group_by(element_id) %>% 
    summarize(meanSentiment = mean(sentiment))
  head(tweet_sentences_data)
  
  
  df1 <- cbind.data.frame(df, output_lsd, tweet_sentences_data)
  
  # df <- cbind.data.frame(df, tweet_sentences_data)
  
  assign(eval(tweets_name_clean), df1)
  
  # rm(test, tweets, tweet_files,
  #    bearer_token, consumer_key, consumer_secret)
  rm(list = ls()[ls()!=eval(tweets_name_clean)&ls()!="tweets_name_clean"])
  
  save(list = ls(), file = paste("../Data_for_ShinyApp/MeToo_tweets/MFT_coded/", tweets_name_clean, ".RData", sep = ""))
  rm(list = ls())
  
}

lapply(2:67, month_MFT_fun)
# 
# month_MFT_fun(2)
# 
# path <- "../Data_for_ShinyApp/MeToo_tweets/MFT_coded/"
# #path <- list.files("../Data_for_ShinyApp/MeToo_tweets/MFT_coded/")
# get(load(paste0(path,"/",list.files(path)[2]))[1])
# 
# variable.names(MFT_clean_tweets_2017_09_September)

