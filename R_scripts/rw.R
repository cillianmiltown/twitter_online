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


load("../confidential/twitter_dev_credentials.RData")
bearer_token <- Sys.getenv("$BEARER_TOKEN")
headers <- c(`Authorization` = sprintf('Bearer %s', bearer_token))


bearer_token <- 
  "AAAAAAAAAAAAAAAAAAAAAJosOwEAAAAANAZI%2BmqUcoZl8LUr%2FlvyvR3clXE%3DC3RyqBlvYFs0QGOJzDMVTkhoV92B9fwBc9dRo5bB9jqdXPwbLi"

params <- list(`user.fields` = 'description',
               `expansions` = 'pinned_tweet_id')



readline('cillianmacaodh')
handle <- readline('cillianmacaodh')
url_handle <-
  sprintf('https://api.twitter.com/2/users/by?usernames=%s', 'cillianmacaodh')

url_handle

response <-
  httr::GET(url = url_handle,
            httr::add_headers(.headers = headers),
            query = params)
obj <- httr::content(response, as = "text")
print(obj)


get_bearer()




tweets <- tweets[which(tweets$dup_id==1),]
tweets$author_id[2]


test <- get_user_profile(tweets$author_id[2],bearer_token)



#install.packages("osfr")

library(osfr)
library(tidyverse)

load(url("https://www.dropbox.com/s/air9awpefrl0xvb/clean_tweets_2017_10_October.RData?dl=1"))

load(url("https://osf.io/d8tny?dl=1"))

https://osf.io/d8tny


osfr::osf_retrieve_node("https://osf.io/d8tny")

osfr::osf_retrieve_node("https://osf.io/y236d")



load(url("https://osf.io/y236d/files/osfstorage/621fcfc0295ed1025c2818e7"))



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
