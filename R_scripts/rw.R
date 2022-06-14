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
