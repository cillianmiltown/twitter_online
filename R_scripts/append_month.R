library("rdrop2")
library(tidyverse)
library(academictwitteR)
library(jsonlite)
library(data.table)

rm(list = ls())


all_tweets <-
  (function(){
    
    
    
    load("downloaded_tweets/monthly_data/2017_12_December.RData")
    load("downloaded_tweets/monthly_data/2017_11_November.RData")
    load("downloaded_tweets/monthly_data/2017_10_October.RData")
    load("downloaded_tweets/monthly_data/2017_09_September.RData")
    load("downloaded_tweets/monthly_data/2018_01_January.RData")
    load("downloaded_tweets/monthly_data/2018_03_March.RData")
    load("downloaded_tweets/monthly_data/2018_02_February.RData")
    load("downloaded_tweets/monthly_data/2018_04_April.RData")
    load("downloaded_tweets/monthly_data/2018_07_July.RData")
    load("downloaded_tweets/monthly_data/2018_08_August.RData")
    load("downloaded_tweets/monthly_data/2018_09_September.RData")
    load("downloaded_tweets/monthly_data/2018_10_October.RData")
    load("downloaded_tweets/monthly_data/2018_05_May.RData")
    load("downloaded_tweets/monthly_data/2018_06_June.RData")
    load("downloaded_tweets/monthly_data/2018_11_November.RData")
    load("downloaded_tweets/monthly_data/2018_12_December.RData")
    load("downloaded_tweets/monthly_data/2019_01_January.RData")
    load("downloaded_tweets/monthly_data/2019_02_February.RData")
    load("downloaded_tweets/monthly_data/2019_03_March.RData")
    load("downloaded_tweets/monthly_data/2019_04_April.RData")
    load("downloaded_tweets/monthly_data/2019_05_May.RData")
    load("downloaded_tweets/monthly_data/2019_06_June.RData")
    load("downloaded_tweets/monthly_data/2019_07_July.RData")
    load("downloaded_tweets/monthly_data/2019_08_August.RData")
    load("downloaded_tweets/monthly_data/2019_09_September.RData")
    load("downloaded_tweets/monthly_data/2019_10_October.RData")
    load("downloaded_tweets/monthly_data/2019_11_November.RData")
    load("downloaded_tweets/monthly_data/2019_12_December.RData")
    load("downloaded_tweets/monthly_data/2020_01_January.RData")
    load("downloaded_tweets/monthly_data/2020_02_February.RData")
    load("downloaded_tweets/monthly_data/2020_03_March.RData")
    load("downloaded_tweets/monthly_data/2020_04_April.RData")
    load("downloaded_tweets/monthly_data/2020_05_May.RData")
    load("downloaded_tweets/monthly_data/2020_06_June.RData")
    load("downloaded_tweets/monthly_data/2020_07_July.RData")
    load("downloaded_tweets/monthly_data/2020_08_August.RData")
    load("downloaded_tweets/monthly_data/2020_09_September.RData")
    load("downloaded_tweets/monthly_data/2020_10_October.RData")
    load("downloaded_tweets/monthly_data/2020_11_November.RData")
    load("downloaded_tweets/monthly_data/2020_12_December.RData")
    load("downloaded_tweets/monthly_data/2021_01_January.RData")
    load("downloaded_tweets/monthly_data/2021_02_February.RData")
    load("downloaded_tweets/monthly_data/2021_03_March.RData")
    load("downloaded_tweets/monthly_data/2021_04_April.RData")
    load("downloaded_tweets/monthly_data/2021_05_May.RData")
    load("downloaded_tweets/monthly_data/2021_06_June.RData")
    load("downloaded_tweets/monthly_data/2021_07_July.RData")
    load("downloaded_tweets/monthly_data/2021_08_August.RData")
    load("downloaded_tweets/monthly_data/2021_09_September.RData")
    load("downloaded_tweets/monthly_data/2021_10_October.RData")
    load("downloaded_tweets/monthly_data/2021_11_November.RData")
    load("downloaded_tweets/monthly_data/2021_12_December.RData")
    load("downloaded_tweets/monthly_data/2022_01_January.RData")
    
    
    
    
    
    # rm(test)
    all_tweets <- rbind(
      tweets_2017_09_September
      ,tweets_2017_10_October
      ,tweets_2017_11_November
      ,tweets_2017_12_December
      ,tweets_2018_01_January
      ,tweets_2018_02_February
      ,tweets_2018_03_March
      ,tweets_2018_04_April
      ,tweets_2018_05_May
      ,tweets_2018_06_June
      ,tweets_2018_07_July
      ,tweets_2018_08_August
      ,tweets_2018_09_September
      ,tweets_2018_10_October
      ,tweets_2018_11_November
      ,tweets_2018_12_December
      ,tweets_2019_01_January
      ,tweets_2019_02_February
      ,tweets_2019_03_March
      ,tweets_2019_04_April
      ,tweets_2019_05_May
      ,tweets_2019_06_June
      ,tweets_2019_07_July
      ,tweets_2019_08_August
      ,tweets_2019_09_September
      ,tweets_2019_10_October
      ,tweets_2019_11_November
      ,tweets_2019_12_December
      ,tweets_2020_01_January
      ,tweets_2020_02_February
      ,tweets_2020_03_March
      ,tweets_2020_04_April
      ,tweets_2020_05_May
      ,tweets_2020_06_June
      ,tweets_2020_07_July
      ,tweets_2020_08_August
      ,tweets_2020_09_September
      ,tweets_2020_10_October
      ,tweets_2020_11_November
      ,tweets_2020_12_December
      ,tweets_2021_01_January
      ,tweets_2021_02_February
      ,tweets_2021_03_March
      ,tweets_2021_04_April
      ,tweets_2021_05_May
      ,tweets_2021_06_June
      ,tweets_2021_07_July
      ,tweets_2021_08_August
      ,tweets_2021_09_September
      ,tweets_2021_10_October
      ,tweets_2021_11_November
      ,tweets_2021_12_December
      ,tweets_2022_01_January
    )
    all_tweets
  })()



all_tweets_table <- as.data.table(all_tweets)
 
fwrite(all_tweets_table, "MeToo_tweets/all_tweets.csv")


save(all_tweets, file = "downloaded_tweets/all_data/all_tweets.RData")


##### set up for plot #####
rm(all_tweets_table)

df <- all_tweets
rm(all_tweets)


#sum(df$`date <- as.Date(created_at)`=="2017-10-16")


y <- table(df$date)

y <- as.data.frame(y)
colnames(y) <- c("date","Freq")
class(y$date)
y$date <- as.Date(y$date)

save(y, file = "MeToo_tweets/data_for_plot.RData")


