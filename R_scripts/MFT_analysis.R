rm(list = ls())
#install.packages("devtools")
#devtools::install_github("kbenoit/quanteda.dictionaries")
library("quanteda")
library("quanteda.dictionaries")
library(sentimentr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)




month_MFT_fun_analysis <- function(x1){
  path <- "../Data_for_ShinyApp/MeToo_tweets/MFT_coded/"
  tweet_files <- list.files(path)
  
  month_data <- get(load(paste0(path,"/",list.files(path)[x1]))[1])
  tweets_name_clean <- paste0("MFT_", gsub(".RData", "", tweet_files[x1]))
  
  df <- month_data
  
  
  df1 <- df %>% group_by(date) %>% 
    summarize(  mean_daily_sentiment = mean(meanSentiment)
              , total_tweets = length(dup_id)
              , total_twwts_incl_retweets = sum(num_dups)
              , care_virtue = mean(care.virtue)
              , care_vice = mean(care.vice)
              , fairness_virtue = mean(fairness.virtue)
              , fairness_vice = mean(fairness.vice)
              , loyalty_virtue = mean(loyalty.virtue)
              , loyalty_vice = mean(loyalty.vice)
              , authority_virtue = mean(authority.virtue)
              , authority_vice = mean(authority.vice)
              , sanctity_virtue = mean(sanctity.virtue)
              , sanctity_vice = mean(sanctity.vice)
    )
  
  df1
}

month_MFT_fun_analysis(2)

MFT_overall <- do.call(rbind, lapply(2:67, month_MFT_fun_analysis))

MFT_overall_long <- 
  MFT_overall %>% pivot_longer(
    c(
      "mean_daily_sentiment",
      # "total_tweets",
      # "total_twwts_incl_retweets",
      "care_virtue",
      "care_vice",
      "fairness_virtue",
      "fairness_vice",
      "loyalty_virtue",
      "loyalty_vice",
      "authority_virtue",
      "authority_vice",
      "sanctity_virtue",
      "sanctity_vice"
    ),
    names_to='measure',
    values_to='value')


df <- MFT_overall_long
class(df$measure)
df$measure <- factor(df$measure,
                     levels = c(
                       "mean_daily_sentiment",
                       "care_virtue",
                       "care_vice",
                       "fairness_virtue",
                       "fairness_vice",
                       "loyalty_virtue",
                       "loyalty_vice",
                       "authority_virtue",
                       "authority_vice",
                       "sanctity_virtue",
                       "sanctity_vice"
                     )
                     )

levels_df <- levels(df$measure)
colors_df <- recode(levels_df
                    ,"mean_daily_sentiment"="#ffeb3b"
                    ,"care_virtue" = "#ef5350"
                    ,"care_vice" = "#ec407a"
                    ,"fairness_virtue" = "#81c784"
                    ,"fairness_vice" = "#64ffda"
                    ,"loyalty_virtue" = "#29b6f6"
                    ,"loyalty_vice" = "#82b1ff"
                    ,"authority_virtue" = "#ffa726"
                    ,"authority_vice" = "#ff7043"
                    ,"sanctity_virtue" = "#ba68c8"
                    ,"sanctity_vice" = "#ce93d8"
                    )


ggplot(df, aes(date, value, color = measure)) + 
   scale_color_manual(values = colors_df)+
  geom_line(linewidth=.2)+
  #geom_line(aes(date, mean_daily_sentiment))+
  theme_bw()




save(MFT_overall_long, file = "MeToo_tweets_online_Shiny/MFT_overall_long.RData")



# path <- "../Data_for_ShinyApp/MeToo_tweets/MFT_coded/"
# tweet_files <- list.files(path)
# get(load(paste0(path,"/",list.files(path)[2]))[1])
# variable.names(MFT_clean_tweets_2017_09_September)
