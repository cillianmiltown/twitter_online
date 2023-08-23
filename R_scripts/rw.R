library("rdrop2")
library(tidyverse)
library(academictwitteR)
library(jsonlite)
library(data.table)

rm(list = ls())
token <- readRDS("../confidential/token.rds")

#token <- drop_auth()

getwd()
x <- 20
#select_month <- function(x){
parent_metoo <- drop_dir("Online Only/R/twitter_data/MeToo", dtoken = token)#, dtoken = token)
month_online <- drop_dir(parent_metoo$path_display[x], dtoken = token)

#within_month_dir <- function(x){

local_path2 <- paste("downloaded_tweets/temp/",month_online$name[x], sep = "")
drop_download(month_online$path_display[x], local_path = local_path2, overwrite = TRUE, dtoken = token)

tweets <- readRDS(local_path2)
tweets <- tweets[which(str_detect(tweets$referenced_tweets, "retweeted")==FALSE),]

tweets_export <- tweets %>% select(
  author_id
  , id
  , text  
  , public_metrics
  , created_at
  , lang
  
  #  , public_metrics$reply_count
) %>% mutate(
  date <- as.Date(created_at)
)
file.remove(local_path2)
rm(tweets)
tweets_export <- `rownames<-`(tweets_export, paste(tweets_export$date,row.names(tweets_export)))
tweets_export$public_metrics <- `rownames<-`(tweets_export$public_metrics, paste(tweets_export$date,row.names(tweets_export$public_metrics)))
tweets_export
#}

month_x <- do.call("rbind", lapply(1:length(month_online$name), within_month_dir))
month_x
#}


load("downloaded_tweets/monthly_data/2021_03_March.RData")

load("MeToo_tweets/data_for_plot.RData")


formatC(sum(y$Freq), big.mark=",")
load(url("https://www.dropbox.com/s/s3t1rqfic21s3pu/clean_tweets_2017_09_September.RData?dl=1"))

x <- month[19]
file_name_upload <- paste(x,".rds",sep = "")
drop_upload(file_name_upload,"Online Only/R/twitter_data/MeToo/2022_05_May", dtoken = token)
file.remove(file_name_upload)


##### Aug 2023 #####

academictwitteR::get_bearer()
academictwitteR::
  
  
  # install.packages("httr")
  # install.packages("jsonlite")
  # install.packages("dplyr")
  
  
  require(httr)
require(jsonlite)
require(dplyr)

# 
# params <- list(`user.fields` = 'description',
#                `expansions` = 'pinned_tweet_id')
# 
# 
# 
# readline('cillianmacaodh')
# handle <- readline('cillianmacaodh')
# url_handle <-
#   sprintf('https://api.twitter.com/2/users/by?usernames=%s', 'cillianmacaodh')
# 
# url_handle
# 
# response <-
#   httr::GET(url = url_handle,
#             httr::add_headers(.headers = headers),
#             query = params)
# obj <- httr::content(response, as = "text")
# print(obj)
# 
# 
# get_bearer()
# 
# 
# 
# 
# tweets <- tweets[which(tweets$dup_id==1),]
# tweets$author_id[2]
# 
# 
# test <- get_user_profile(tweets$author_id[2],bearer_token)
# 
# 
# 
# #install.packages("osfr")
# 
# library(osfr)
# library(tidyverse)
# 
# load(url("https://www.dropbox.com/s/air9awpefrl0xvb/clean_tweets_2017_10_October.RData?dl=1"))
# 
# load(url("https://osf.io/d8tny?dl=1"))
# 
# https://osf.io/d8tny
# 
# 
# osfr::osf_retrieve_node("https://osf.io/d8tny")
# 
# osfr::osf_retrieve_node("https://osf.io/y236d")
# 
# 
# 
# load(url("https://osf.io/y236d/files/osfstorage/621fcfc0295ed1025c2818e7"))



osf_retrieve_node("jgyxm") %>%
  osf_ls_files() %>%
  osf_download()


osf_retrieve_node("d8tny") %>%
  osf_ls_files() %>%
  osf_download()


osf_metoo_project <- osfr::osf_retrieve_node("https://osf.io/y236d")

osf_ls_files(osf_metoo_project)


load(url("https://osf.io/y236d/files/osfstorage/621fcfbfc06427024cd908fe?dl=1"))

rm(list = ls())
load(url("https://osf.io/d8tny/download"))





##### app not working #### 
library(shiny)
#install.packages("shinydashboard")
library(shinydashboard)

#library(shiny)
library("rdrop2")
library(tidyverse)
#library(shinydashboard)
library(academictwitteR)
library(data.table)


### Raw
# May 2018
load(url("https://osf.io/gvfm6/download"))

# Feb 2022
load(url("https://osf.io/yxcm5/download"))


# March 2022
load(url("https://osf.io/kdjs7/download"))


# March - Dec 2022
load(url("https://osf.io/kdjs7/download"))
load(url("https://osf.io/yqd4b/download"))
load(url("https://osf.io/rh2ga/download"))
load(url("https://osf.io/mvbr7/download"))
load(url("https://osf.io/gkz9t/download"))
load(url("https://osf.io/eqtu4/download"))
load(url("https://osf.io/9t6uq/download"))
load(url("https://osf.io/52exq/download"))
load(url("https://osf.io/gfba5/download"))
load(url("https://osf.io/5k3du/download"))


# Jan - Feb 2023
load(url("https://osf.io/2e79r/download"))
load(url("https://osf.io/2g3rm/download"))



variable.names(tweets_2022_03_March)

#https://osf.io/gvfm6
df1 <- tweets_2023_01_January


ls(pattern = "tweets_*")
loaded_months <- ls(pattern = "tweets_*")
df1 <- do.call("rbind", mget(loaded_months))

df1$date <- df1$`date <- as.Date(created_at)`

Author <- `colnames<-`(
  as.data.table(df1$author_id)
  , "Author")
ID <- `colnames<-`(
  as.data.table(df1$id)
  , "ID")
link <-  `colnames<-`(
  as.data.table(df1$url)
  , "link")
Content <- `colnames<-`(
  as.data.table(df1$text)
  , "Tweet Content")
Metrics <- `colnames<-`(
  as.data.table(df1$public_metrics[1:4])
  , c("retweet_count","reply_count","like_count","quote_count"))
date_tag  <- `colnames<-`(
  as.data.table(df1$created_at)
  , "Date (metata)")
date_simple <- `colnames<-`(
  as.data.table(df1$date)
  , "Date (simple)")
language <- `colnames<-`(
  as.data.table(df1$lang)
  , "Language")


### Clean


# Jan - Dec 2022
load(url("https://osf.io/5qywm/download"))
load(url("https://osf.io/ka3e7/download"))
load(url("https://osf.io/f7me2/download"))
load(url("https://osf.io/n3u5t/download"))
load(url("https://osf.io/npjyv/download"))
load(url("https://osf.io/5m89c/download"))
load(url("https://osf.io/25k3r/download"))
load(url("https://osf.io/z7dwv/download"))
load(url("https://osf.io/w7hqz/download"))
load(url("https://osf.io/2fh4w/download"))
load(url("https://osf.io/uj3q5/download"))
load(url("https://osf.io/eszp5/download"))


# Jan - Feb 2023
load(url("https://osf.io/2g6zc/download"))
load(url("https://osf.io/px2n3/download"))

variable.names(tweets_2023_01_January)



### 21/Aug/2023

# https://towardsdatascience.com/an-intro-to-sentiment-analysis-in-r-how-does-twitter-feel-about-baker-mayfield-cda513ed0b78

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
library(sentimentr)


df <- clean_tweets_2023_01_January


tweets_data <- clean_tweets_2023_01_January[which(clean_tweets_2023_01_January$lang=="en"),]
tweets_data$text

df <- clean_tweets_2023_01_January
df <- df[which(df$lang=="en"),]
df <- df[!grepl("Rajokri", df$text),]
df[!grepl("Rajokri", df$text),]

df <- df %>% 
  mutate(text = str_replace(text, "&amp;", " and "))
df$text

df$text
grepl("Rajokri", df$text)
grepl("noxious pesticides being released", df$text)

tweets_data <- df
tweets_data[1000,]

words_data <- tweets_data %>% select(text)  %>% 
 unnest_tokens(word, text)

words_data %>% count(word, sort = TRUE)
words_data <- words_data %>% filter(!word %in% c('https', 't.co', 'he\'s', 'i\'m', 'it\'s'))

words_data2 <- words_data %>%
  anti_join(stop_words) %>%
  count(word, sort = TRUE)
head(words_data2, n = 20)
tail(words_data2, n = 10)


words_data2 %>%
  inner_join(get_sentiments("bing"),multiple = "all") %>%
  count(sentiment, sort = TRUE)

table(words_data2$word)
sum(words_data2$n==3)
df <- words_data2


head(df, n = 20)
tail(df, n = 10)


df <- df[which(df$n>3),]

df1 <- df[2:100,]
df$word

wordcloud::wordcloud(df1$word, df1$n)


#install.packages("sentimentr")
library(sentimentr)



df <- clean_tweets_2023_01_January
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

df$element_id
df1 <- df %>% group_by(date) %>% 
  summarize(mean_daily_sentiment = mean(meanSentiment)
            , total_tweets = length(dup_id)
            , total_twwts_incl_retweets = sum(num_dups)
            )


df1
ggplot(df1, aes(date, total_twwts_incl_retweets, fill = mean_daily_sentiment)) + 
  scale_fill_continuous(low = "red",
                        high = "lightblue",)+
  geom_col()+
  theme_bw()



tweets_data$text[2]

print(paste0("Most negative tweets sentiment: ", min(tweet_sentences_data$meanSentiment)))
print(paste0("Most positive tweets sentiment: ", max(tweet_sentences_data$meanSentiment)))
print(paste0("# of Negative Tweets: ", sum(tweet_sentences_data$meanSentiment < 0)))
print(paste0("# of Neutral Tweets: ", sum(tweet_sentences_data$meanSentiment == 0)))
print(paste0("# of Positive Tweets: ", sum(tweet_sentences_data$meanSentiment > 0)))


slices <- c(sum(tweet_sentences_data$meanSentiment < 0), sum(tweet_sentences_data$meanSentiment == 0),
            sum(tweet_sentences_data$meanSentiment > 0))
labels <- c("Negative Tweets: ", "Neutral Tweets: ", "Positive Tweets: ")
pct <- round(slices/sum(slices)*100)
labels <- paste(labels, pct, "%", sep = "")
#customize labeling#add in appropriate colors for positive, neutral, negative
pie(slices, labels = labels, col=c('red', 'yellow', 'green'), 
    main="Tweet Sentiment Percentages")
