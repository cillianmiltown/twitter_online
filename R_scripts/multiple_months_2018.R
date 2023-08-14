# library("rdrop2")
# library(tidyverse)
# library(academictwitteR)
# library(jsonlite)
# library(data.table)


# rm(list = ls())
# 
# token <- drop_auth(new_user = TRUE)
# saveRDS(token, file = "../confidential/token.rds")
# 
# #token <- drop_auth()
# #token$credentials
# 
# 
# #load("../confidential/twitter_dev_credentials.RData")
# #readRDS("../confidential/token.rds")
# token <- readRDS("../confidential/token.rds")




select_month <- function(x){
  parent_metoo <- drop_dir("Online Only/R/twitter_data/MeToo", dtoken = token)
  month_online <- drop_dir(parent_metoo$path_display[x], dtoken = token)
  
  within_month_dir <- function(x){
    
    local_path2 <- paste("../downloaded_tweets/temp/",month_online$name[x], sep = "")
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
  }
  
  month_x <- do.call("rbind", lapply(1:length(month_online$name), within_month_dir))
  month_x
}


tweets_to_dropbox <- function(day,hashtag,number_of_days=1){
  x <- day
  y <- as.Date(x)+number_of_days
  start_date <- paste(x,"T00:00:00Z", sep = "")
  end_date <- paste(y,"T00:00:00Z", sep = "")
  file_name <- x # paste(x,"_tweets",sep="")
  #hashtag <- "#MeToo"
  
  
  tweets <-
    get_all_tweets(
      hashtag,
      start_date,
      end_date,
      bearer_token,
      file = paste(x,".rds",sep = ""),
      #data_path = "data/",
      n = Inf
    )
  
  file_name_upload <- paste(x,".rds",sep = "")
  drop_upload(file_name_upload,"Online Only/R/twitter_data/MeToo/2023_02_February", dtoken = token)
  file.remove(file_name_upload)
}


loop_metoo_tweets <- function(day){
  day_formatted <- as.Date(day,origin="1970-01-01")
  tweets_to_dropbox(day_formatted,"#MeToo",1)
}


#tweets_to_dropbox(month[1],"#MeToo",1)

day_for_fun <- "2023-02-01"
month <- as.Date(day_for_fun) + c(0:28)

sapply(month[1:11], loop_metoo_tweets)


parent_metoo <- drop_dir("Online Only/R/twitter_data/MeToo", dtoken = token)
parent_metoo$path_display
tweets_2023_02_February  <- select_month(67)
save(tweets_2023_02_February, file = "../downloaded_tweets/monthly_data/2023_02_February.RData")

