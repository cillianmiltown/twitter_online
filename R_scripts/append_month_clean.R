library("rdrop2")
library(tidyverse)
library(academictwitteR)
library(jsonlite)
library(data.table)

rm(list = ls())


all_tweets <-
  (function(){

    
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2017_09_September.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2017_10_October.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2017_11_November.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2017_12_December.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2018_01_January.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2018_03_March.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2018_02_February.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2018_04_April.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2018_07_July.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2018_08_August.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2018_09_September.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2018_10_October.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2018_05_May.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2018_06_June.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2018_11_November.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2018_12_December.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2019_01_January.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2019_02_February.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2019_03_March.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2019_04_April.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2019_05_May.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2019_06_June.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2019_07_July.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2019_08_August.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2019_09_September.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2019_10_October.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2019_11_November.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2019_12_December.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2020_01_January.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2020_02_February.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2020_03_March.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2020_04_April.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2020_05_May.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2020_06_June.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2020_07_July.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2020_08_August.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2020_09_September.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2020_10_October.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2020_11_November.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2020_12_December.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2021_01_January.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2021_02_February.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2021_03_March.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2021_04_April.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2021_05_May.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2021_06_June.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2021_07_July.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2021_08_August.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2021_09_September.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2021_10_October.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2021_11_November.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2021_12_December.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2022_01_January.RData")
    load("../downloaded_tweets/cleaned_monthly_data/clean_tweets_2022_02_February.RData")
    
    
    
    
    
    # rm(test)
    all_tweets <- rbind(
        clean_tweets_2017_09_September
      ,clean_tweets_2017_10_October
      ,clean_tweets_2017_11_November
      ,clean_tweets_2017_12_December
      ,clean_tweets_2018_01_January
      ,clean_tweets_2018_02_February
      ,clean_tweets_2018_03_March
      ,clean_tweets_2018_04_April
      ,clean_tweets_2018_05_May
      ,clean_tweets_2018_06_June
      ,clean_tweets_2018_07_July
      ,clean_tweets_2018_08_August
      ,clean_tweets_2018_09_September
      ,clean_tweets_2018_10_October
      ,clean_tweets_2018_11_November
      ,clean_tweets_2018_12_December
      ,clean_tweets_2019_01_January
      ,clean_tweets_2019_02_February
      ,clean_tweets_2019_03_March
      ,clean_tweets_2019_04_April
      ,clean_tweets_2019_05_May
      ,clean_tweets_2019_06_June
      ,clean_tweets_2019_07_July
      ,clean_tweets_2019_08_August
      ,clean_tweets_2019_09_September
      ,clean_tweets_2019_10_October
      ,clean_tweets_2019_11_November
      ,clean_tweets_2019_12_December
      ,clean_tweets_2020_01_January
      ,clean_tweets_2020_02_February
      ,clean_tweets_2020_03_March
      ,clean_tweets_2020_04_April
      ,clean_tweets_2020_05_May
      ,clean_tweets_2020_06_June
      ,clean_tweets_2020_07_July
      ,clean_tweets_2020_08_August
      ,clean_tweets_2020_09_September
      ,clean_tweets_2020_10_October
      ,clean_tweets_2020_11_November
      ,clean_tweets_2020_12_December
      ,clean_tweets_2021_01_January
      ,clean_tweets_2021_02_February
      ,clean_tweets_2021_03_March
      ,clean_tweets_2021_04_April
      ,clean_tweets_2021_05_May
      ,clean_tweets_2021_06_June
      ,clean_tweets_2021_07_July
      ,clean_tweets_2021_08_August
      ,clean_tweets_2021_09_September
      ,clean_tweets_2021_10_October
      ,clean_tweets_2021_11_November
      ,clean_tweets_2021_12_December
      ,clean_tweets_2022_01_January
      ,clean_tweets_2022_02_February
    )
    all_tweets
  })()



all_clean_tweets_table <- as.data.table(all_tweets)
 
#fwrite(all_clean_tweets_table, "../Data_for_ShinyApp/MeToo_tweets/clean/clean_data_for_plot.RData")

#fwrite(all_clean_tweets_table, "MeToo_tweets/all_clean_tweets.csv")


save(all_clean_tweets_table, file = "../downloaded_tweets/all_data/all_clean_tweets.RData")


##### set up for plot #####
rm(all_clean_tweets_table)

df <- all_tweets
rm(all_tweets)


#sum(df$`date <- as.Date(created_at)`=="2017-10-16")


y <- table(df$date)

y <- as.data.frame(y)
colnames(y) <- c("date","Freq")
class(y$date)
y$date <- as.Date(y$date)

y_clean <- y

save(y_clean, file = "../Data_for_ShinyApp/MeToo_tweets/clean/clean_data_for_plot.RData")


