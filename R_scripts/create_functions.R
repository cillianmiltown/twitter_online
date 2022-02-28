library("rdrop2")
library(tidyverse)
library(academictwitteR)



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
      file = file_name
    )
  
  file_name_upload <- paste(x,".rds",sep = "")
  drop_upload(file_name_upload,"Online Only/R/twitter_data/MeToo/October_2017")
  file.remove(file_name_upload)
}


loop_metoo_tweets <- function(day){
  day_formatted <- as.Date(day,origin="1970-01-01")
  tweets_to_dropbox(day_formatted,"#MeToo",1)
}


loop

day_for_fun <- "2017-10-01"
October <- as.Date(day_for_fun) + c(0:30)
#October <- as.vector(October)
#October <- as.list(October)

sapply(October, loop_metoo_tweets)

loop_metoo_tweets(October[1])

sapply(October, test_date)

###### RW ######

test_date <- function(day){
  as.Date(day,origin="1970-01-01")+365
  }
  

September_online <- drop_dir("Online Only/R/twitter_data/MeToo/September_2017")
local_path1 <- paste("downloaded_tweets/temp/",September_online$name[1],sep = "")
drop_download(September_online$path_display[1], local_path = local_path1)
readRDS(local_path1)

#file.remove(September_online$path_display[1])


#drop_download("Online Only/R/twitter_data/MeToo/September_2017", local_path = local_path1)

readRDS(filename)


