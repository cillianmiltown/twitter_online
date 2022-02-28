#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library("rdrop2")
library(tidyverse)
library(academictwitteR)
library(data.table)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("#MeToo Tweets"),
    tabsetPanel(type = "tabs",
                ##### raw data #####      
                tabPanel(
                    "Table of Raw Tweets",
                    
                    
                    # Sidebar with a slider input for number of bins 
                    sidebarLayout(
                        sidebarPanel(
                            dateRangeInput('dateRange',
                                           label = 'Select Date Range:',
                                           start = "2022-01-01", end = "2022-01-31"
                            ),
                            # numericInput("inNumber", "Randomly Sample", 30)
                            # ,
                            # numericInput("inNumber2", "To Participant number:", 2)
                            # ,
                            HTML("Please note: <br>",
                                 "1. Due to large volume of data selecting a narrow date range is recommended<br>",
                                 "2. Selecting a range across months can cause errors<br>",
                                 "&nbsp;"),
                            checkboxGroupInput("cols_select", "Select Columns",
                                               choiceNames = c("Author ID",
                                                               "Tweet ID",
                                                               "Link",
                                                               "Tweet Content",
                                                               "Metrics",
                                                               "Created",
                                                               "Date",
                                                               "Language"),
                                               choiceValues = c(
                                                   "author"       
                                                   ,"id"
                                                   ,"link"
                                                   ,"text"                        
                                                   ,"public_metrics"             
                                                   ,"created_at"
                                                   ,"date_simple"
                                                   ,"lang"),
                                               selected = c("text")
                            )#,
                            # sliderInput("bins",
                            #             "Number of bins:",
                            #             min = 1,
                            #             max = 50,
                            #             value = 30)
                            
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                            #plotOutput("distPlot"),
                            #plotOutput("plot2"),
                            DT::dataTableOutput("table1")
                        )
                    )
                )
                ,
                ##### Plot #####      
                tabPanel(
                    "Plot",


                    # Sidebar with a slider input for number of bins
                    sidebarLayout(
                        sidebarPanel(
                            dateRangeInput('dateRange2',
                                           label = 'Select Date Range',
                                           start = "2017-10-01", end = Sys.Date()
                             ),
                            textOutput("text")#,
                            # # numericInput("inNumber", "From Participant number:", 1)
                            # # ,
                            # # numericInput("inNumber2", "To Participant number:", 2)
                            # # ,
                            # checkboxGroupInput("cols_select", "Select Columns",
                            #                    choiceNames = c("Author",
                            #                                    "ID",
                            #                                    "Tweet Content",
                            #                                    "Metrics",
                            #                                    "Created",
                            #                                    "date"),
                            #                    choiceValues = c(
                            #                        "author"
                            #                        ,"id"
                            #                        ,"text"
                            #                        ,"public_metrics"
                            #                        ,"created_at"
                            #                        ,"date_simple")
                            # ),

                        #     sliderInput("bins",
                        #                 "Number of bins:",
                        #                 min = 1,
                        #                 max = 50,
                        #                 value = 30)
                         ),

                        # Show a plot of the generated distribution
                        mainPanel(
                            #plotOutput("distPlot"),
                            plotOutput("plot2")#,
                            #DT::dataTableOutput("table1")
                        )
                    )
                )
                ,
                ##### clean data #####      
                tabPanel(
                    "Table of Cleaned Tweets",
                    
                    
                    # Sidebar with a slider input for number of bins 
                    sidebarLayout(
                        sidebarPanel(
                            dateRangeInput('dateRange3',
                                           label = 'Select Date Range:',
                                           start = "2022-01-01", end = "2022-01-31"
                            ),
                            # numericInput("inNumber", "Randomly Sample", 30)
                            # ,
                            # numericInput("inNumber2", "To Participant number:", 2)
                            # ,
                            HTML("Please note: <br>",
                                "1. Due to large volume of data selecting a narrow date range is recommended<br>",
                                "2. Selecting a range across months can cause errors<br>",
                                "&nbsp;"),
                            checkboxGroupInput("cols_select2", "Select Columns",
                                               choiceNames = c("Author ID",
                                                               "Tweet ID",
                                                               "Link",
                                                               "Tweet Content",
                                                               "Metrics",
                                                               "Created",
                                                               "Date",
                                                               "Language"),
                                               choiceValues = c(
                                                   "author"       
                                                   ,"id"
                                                   ,"link"
                                                   ,"text"                        
                                                   ,"public_metrics"             
                                                   ,"created_at"
                                                   ,"date_simple"
                                                   ,"lang"),
                                               selected = c("text")
                            )#,
                            # sliderInput("bins",
                            #             "Number of bins:",
                            #             min = 1,
                            #             max = 50,
                            #             value = 30)
                            
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                            #plotOutput("distPlot"),
                            #plotOutput("plot2"),
                            DT::dataTableOutput("table2")
                        )
                    )
                )
                ))

# Define server logic required to draw a histogram
server <- function(input, output) {

    #all_tweets <- fread("all_tweets.csv")
    
    make_a <- reactive({input$inNumber})
    # make_b <- reactive({input$inNumber2})
    
    make_start_date <- reactive({input$dateRange[1]})
    make_end_date <- reactive({input$dateRange[2]})
    
    make_start_date2 <- reactive({input$dateRange2[1]})
    make_end_date2 <- reactive({input$dateRange2[2]})
    
    make_start_date3 <- reactive({input$dateRange3[1]})
    make_end_date3 <- reactive({input$dateRange3[2]})
    
    #all_tweets <- fread("MeToo_tweets/all_tweets.csv")
    #df1 <- all_tweets
    
    
    
    mft_boxes1 <- reactive({
        #mft_boxes <- paste(input$MFT, collapse = ", ")
        #paste("You chose", mft_boxes, class(mft_boxes))
        #if (str_detect(mft_boxes, "mft1")) print("boom")
        mft_boxes <- eval(parse(text = 'input$cols_select'))
        #mft_boxes <- input$MFT
        mft_boxes
    })

    mft_boxes2 <- reactive({
        #mft_boxes <- paste(input$MFT, collapse = ", ")
        #paste("You chose", mft_boxes, class(mft_boxes))
        #if (str_detect(mft_boxes, "mft1")) print("boom")
        mft_boxes <- eval(parse(text = 'input$cols_select2'))
        #mft_boxes <- input$MFT
        mft_boxes
    })
    
    # #    mft_boxes <- output$text
    # big_table <- reactive({
    # list_df_fun <- function(){
    #     
    #     list.df = " "
    #     
    #     
    #     if (str_detect(mft_boxes, "author_id")) { 
    #         list.df=c(list.df,deparse(substitute(df_a)))
    #     }
    #     
    #     if (str_detect(mft_boxes, "id")) { 
    #         list.df=c(list.df,deparse(substitute(df_b)))
    #     }
    #     
    #     if (str_detect(mft_boxes, "text")) { 
    #         list.df=c(list.df,deparse(substitute(df_c)))
    #     }
    #     
    #     if (str_detect(mft_boxes, "public_metrics")) { 
    #         list.df=c(list.df,deparse(substitute(df_d)))
    #     }
    #     
    #     if (str_detect(mft_boxes, "created_at")) { 
    #         list.df=c(list.df,deparse(substitute(df_e)))
    #     }
    #     
    #     if (str_detect(mft_boxes, "date <- as.Date(created_at)")) { 
    #         list.df=c(list.df,deparse(substitute(df_f)))
    #     }
    #     
    #     list.df
    # }
    # 
    # list.df <- list_df_fun()
    # 
    # #list.df <- c(" ", deparse(substitute(df_a)),deparse(substitute(df_a)))
    # 
    # list.df = list.df[2:length(list.df)] #remove first element 
    # expression = paste0("df_combined = rbind(",paste0( list.df, collapse = ','), paste0(")"))
    # 
    # eval(parse(text=expression))
    # 
    # df_combined
    # })
    
    
    # df1 <- 
    #     df_table %>% 
    #     select(matches('cols_select')
    #            #,-matches('_DO_')
    #     )
    output$table1 <- DT::renderDataTable({
        
        
        
        
        start_date <- make_start_date()
        end_date <- make_end_date()
        
        
        months_selected <- seq(as.Date(start_date), as.Date(end_date),by="month")
        
        
        ## 2017 ##
        if (grepl("2017-09",months_selected)){
            load("raw/2017_09_September.RData")
        }
        if (grepl("2017-10",months_selected)){
            load("raw/2017_10_October.RData")
        }
        if (grepl("2017-11",months_selected)){
            load("raw/2017_11_November.RData")
        }
        if (grepl("2017-12",months_selected)){
            load("raw/2017_12_December.RData")
        }
        
        ## 2018 ##
        if (grepl("2018-01",months_selected)){    
            load("raw/2018_01_January.RData")
        }
        if (grepl("2018-02",months_selected)){
            load("raw/2018_02_February.RData")
        }
        if (grepl("2018-03",months_selected)){
            load("raw/2018_03_March.RData")
        }
        if (grepl("2018-04",months_selected)){
            load("raw/2018_04_April.RData")
        }
        if (grepl("2018-05",months_selected)){
            load("raw/2018_05_May.RData")
        }
        if (grepl("2018-06",months_selected)){
            load("raw/2018_06_June.RData")
        }
        if (grepl("2018-07",months_selected)){
            load("raw/2018_07_July.RData")
        }
        if (grepl("2018-08",months_selected)){
            load("raw/2018_08_August.RData")
        }
        if (grepl("2018-09",months_selected)){
            load("raw/2018_09_September.RData")
        }
        if (grepl("2018-10",months_selected)){
            load("raw/2018_10_October.RData")
        }
        if (grepl("2018-11",months_selected)){
            load("raw/2018_11_November.RData")
        }
        if (grepl("2018-12",months_selected)){
            load("raw/2018_12_December.RData")
        }
        
        ## 2019 ##
        if (grepl("2019-01",months_selected)){
            load("raw/2019_01_January.RData")
        }
        if (grepl("2019-02",months_selected)){
            load("raw/2019_02_February.RData")
        }
        if (grepl("2019-03",months_selected)){
            load("raw/2019_03_March.RData")
        }
        if (grepl("2019-04",months_selected)){
            load("raw/2019_04_April.RData")
        }
        if (grepl("2019-05",months_selected)){
            load("raw/2019_05_May.RData")
        }
        if (grepl("2019-06",months_selected)){
            load("raw/2019_06_June.RData")
        }
        if (grepl("2019-07",months_selected)){
            load("raw/2019_07_July.RData")
        }
        if (grepl("2019-08",months_selected)){
            load("raw/2019_08_August.RData")
        }
        if (grepl("2019-09",months_selected)){
            load("raw/2019_09_September.RData")
        }
        if (grepl("2019-10",months_selected)){
            load("raw/2019_10_October.RData")
        }
        if (grepl("2019-11",months_selected)){
            load("raw/2019_11_November.RData")
        }
        if (grepl("2019-12",months_selected)){
            load("raw/2019_12_December.RData")
        }
        
        ## 2020 ##
        if (grepl("2020-01",months_selected)){
            load("raw/2020_01_January.RData")
        }
        if (grepl("2020-02",months_selected)){
            load("raw/2020_02_February.RData")
        }
        if (grepl("2020-03",months_selected)){
            load("raw/2020_03_March.RData")
        }
        if (grepl("2020-04",months_selected)){
            load("raw/2020_04_April.RData")
        }
        if (grepl("2020-05",months_selected)){
            load("raw/2020_05_May.RData")
        }
        if (grepl("2020-06",months_selected)){
            load("raw/2020_06_June.RData")
        }
        if (grepl("2020-07",months_selected)){
            load("raw/2020_07_July.RData")
        }
        if (grepl("2020-08",months_selected)){
            load("raw/2020_08_August.RData")
        }
        if (grepl("2020-09",months_selected)){
            load("raw/2020_09_September.RData")
        }
        if (grepl("2020-10",months_selected)){
            load("raw/2020_10_October.RData")
        }
        if (grepl("2020-11",months_selected)){
            load("raw/2020_11_November.RData")
        }
        if (grepl("2020-12",months_selected)){
            load("raw/2020_12_December.RData")
        }
        
        
        
        if (grepl("2021-01",months_selected)){
            load("raw/2021_01_January.RData")
        }
        if (grepl("2021-02",months_selected)){
            load("raw/2021_02_February.RData")
        }
        if (grepl("2021-03",months_selected)){
            load("raw/2021_03_March.RData")
        }
        if (grepl("2021-04",months_selected)){
            load("raw/2021_04_April.RData")
        }
        if (grepl("2021-05",months_selected)){
            load("raw/2021_05_May.RData")
        }
        if (grepl("2021-06",months_selected)){
            load("raw/2021_06_June.RData")
        }
        if (grepl("2021-07",months_selected)){
            load("raw/2021_07_July.RData")
        }
        if (grepl("2021-08",months_selected)){
            load("raw/2021_08_August.RData")
        }
        if (grepl("2021-09",months_selected)){
            load("raw/2021_09_September.RData")
        }
        if (grepl("2021-10",months_selected)){
            load("raw/2021_10_October.RData")
        }
        if (grepl("2021-11",months_selected)){
            load("raw/2021_11_November.RData")
        }
        if (grepl("2021-12",months_selected)){
            load("raw/2021_12_December.RData")
        }
        
        
        if (grepl("2022-01",months_selected)){
            load("raw/2022_01_January.RData")
        }
        
        
        
        ls(pattern = "tweets_*")
        loaded_months <- ls(pattern = "tweets_*")
        df1 <- do.call("rbind", mget(loaded_months))
        
        df1 <- df1[df1$date >= paste(as.character(start_date)) & df1$date <= paste(as.character(end_date)),]
        
        df1 <- df1[order(df1$created_at), ]
        
        # a1 <- make_a()
        # # b1 <- make_b()
        # # 
        # a <- as.numeric(a1)
        # # b <- as.numeric(b1)
        # 
        # df1 <- df1[sample(1:nrow(df1), a), ]
        # 
        #df1 <- df1[a:b,]
        df1 
        
        url1 <- rep("https://twitter.com/", length(df1$id))
        url2 <- rep("/status/", length(df1$id))


        df1$url <- paste(url1,df1$author_id,url2,df1$id,sep="")

        
        #url1 <- rep("https://twitter.com/anyuser/status/",length(df1$id))
        #df1$url <- paste(url1,df1$id,sep="")
        mft_boxes <- paste(mft_boxes1(), sep = " " , collapse = '')
        
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
            as.data.table(df1$public_metrics)
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
        
        list_df_fun <- function(){
            
            
            
            list.df = " "
            
            
            if (str_detect(mft_boxes, "author")) { 
                list.df=c(list.df,deparse(substitute(Author)))
            }
            
            if (str_detect(mft_boxes, "id")) { 
                list.df=c(list.df,deparse(substitute(ID)))
            }
            
            if (str_detect(mft_boxes, "link")) { 
                list.df=c(list.df,deparse(substitute(link)))
            }
            
            if (str_detect(mft_boxes, "text")) { 
                list.df=c(list.df,deparse(substitute(Content)))
            }
            
            if (str_detect(mft_boxes, "public_metrics")) { 
                list.df=c(list.df,deparse(substitute(Metrics)))
            }
            
            if (str_detect(mft_boxes, "created_at")) { 
                list.df=c(list.df,deparse(substitute(date_tag)))
            }
            
            if (str_detect(mft_boxes, "date_simple")) {
                list.df=c(list.df,deparse(substitute(date_simple)))
            }
            if (str_detect(mft_boxes, "lang")) {
                list.df=c(list.df,deparse(substitute(language)))
            }
            list.df
        }
        
        list.df <- list_df_fun()
        
        #list.df <- c(" ", deparse(substitute(df_a)),deparse(substitute(df_a)))
        
        list.df = list.df[2:length(list.df)] #remove first element 
        expression = paste0("df_combined = cbind(",paste0( list.df, collapse = ','), paste0(")"))
        
        eval(parse(text=expression))
        
        df_combined
    })
    
    output$plot2<-renderPlot({
        load("raw/data_for_plot.RData")
        #df1[order(df1$created_at), ]
        start_date <- make_start_date2()
        end_date <- make_end_date2()

        #y <- table(df1$date)

        # df1 <- y
        # df1 <- df1[df1$date >= paste(as.character(start_date)) & df1$date <= paste(as.character(end_date)),]
        # y <- df1
        # 
        # y <- as.data.frame(y)
        # colnames(y) <- c("date","Freq")
        # class(y$date)
        # y$date <- as.Date(y$date)

        y <- y[y$date >= paste(as.character(start_date)) & y$date <= paste(as.character(end_date)),]


        ggplot(y, aes(x=date, y=Freq
        )) +
            # scale_y_continuous(limits = c(-.03,1),
            #                    labels = percent_format()
            # )+
            geom_col(position = "dodge",
                     color="black",
                     size=.2
            ) +
            # geom_text(#family = "Times",
            #   size=4.2,
            #   aes( label = scales::percent(perc,accuracy=1),
            #        y= perc ),
            #   stat= "identity",
            #   vjust = -.5,
            #   position = position_dodge(.9),
            #   fontface='plain'
            # )+
            # geom_text(#family = "Times",
            #   size=4.2,
        #   aes(label = format(Freq),
        #       y= -3*(..count../100)/(..count..)),
        #   stat= "count",
        #   position = position_dodge(0.9),
        #   #vjust = -.05,
        #   fontface='plain'
        # ) +
        #xlab("Response to Critical Slide") +
        #ylab("Percentage of participants selecting each response")+
        scale_x_date(date_breaks="1 month",date_labels = "%B %Y ")+
            # scale_x_discrete(labels=c("Reasons", "Dumbfounded","Nothing Wrong")) +
            #  scale_fill_grey(start = .5, end = .8) +
            # labs(fill="Condition") +
            #theme_apa() +
            theme_bw() +
            theme(panel.border = element_blank(),
                  axis.line = element_line(size = .2),
                  strip.background  = element_blank(),
                  panel.grid = element_blank(),
                  plot.title=element_text(#family="Times",
                      size=12
                  ),
                  legend.text=element_text(#family="Times",
                      size=8
                  ),
                  legend.title=element_text(#family="Times",
                      size=10
                  ),
                  axis.text=element_text(#family="Times",
                      colour = "black",
                      size=8
                  ),
                  # axis.ticks.x = element_blank(),
                  axis.text.x = element_text(angle = 90),
                  axis.title=element_text(#family="Times",
                      size=12
                  ),
                  strip.text=element_text(#family = "Times",
                      size = 12
                  ),
                  # strip.background = element_rect(fill = "white"),
                  legend.position="right")



    })
    # 
    # 
    
    output$text <- renderText({
        load("raw/data_for_plot.RData")
        
        start_date <- make_start_date2()
        end_date <- make_end_date2()
        y <- y[y$date >= paste(as.character(start_date)) & y$date <= paste(as.character(end_date)),]
        
        
        text_to_display <- 
                paste("Displaying total daily tweets from", 
                      as.character(start_date), " to ", paste(as.character(end_date)),
                      " with a total of ", as.character(formatC(sum(y$Freq), big.mark=",")), " Tweets.")
        
        HTML(paste0((text_to_display)))
        
    })

    output$distPlot <- renderPlot({
        df1[order(df1$created_at), ]
        
        
        start_date <- make_start_date()
        end_date <- make_end_date()
        
        
        df1 <- df1[df1$date >= paste(as.character(start_date)) & df1$date <= paste(as.character(end_date)),]
        
        
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
    
    
    output$table2 <- DT::renderDataTable({
        
        # 
        # if (sum(grepl("2017-10",months_selected))==1){
        #     load("clean/clean_tweets_2017_09_September.RData")
        # }
        # 
        # sum(grepl("2017-10",months_selected))==1
        # 
        
        start_date <- make_start_date3()
        end_date <- make_end_date3()
        # start_date <- "2017-09-01"
        # end_date <- "2022-01-05"
        
        months_selected <- seq(as.Date(start_date), as.Date(end_date),by="month")
        
        # lapply(months_selected, function(x)
        #     if (sum(grepl("2022-01",x)){
        #     load("clean/clean_tweets_2017_09_September.RData")
        # })
        
        ## 2017 ##
         
        if (sum(grepl("2017-09",months_selected))==1){
            load("clean/clean_tweets_2017_09_September.RData")
         }
         
        if (sum(grepl("2017-10",months_selected))==1){
            load("clean/clean_tweets_2017_10_October.RData")
         }
         
        if (sum(grepl("2017-11",months_selected))==1){
            load("clean/clean_tweets_2017_11_November.RData")
         }
         
        if (sum(grepl("2017-12",months_selected))==1){
            load("clean/clean_tweets_2017_12_December.RData")
         }
        
        ## 2018 ##
         
        if (sum(grepl("2018-01",months_selected))==1){    
            load("clean/clean_tweets_2018_01_January.RData")
         }
         
        if (sum(grepl("2018-02",months_selected))==1){
            load("clean/clean_tweets_2018_02_February.RData")
         }
         
        if (sum(grepl("2018-03",months_selected))==1){
            load("clean/clean_tweets_2018_03_March.RData")
         }
         
        if (sum(grepl("2018-04",months_selected))==1){
            load("clean/clean_tweets_2018_04_April.RData")
         }
         
        if (sum(grepl("2018-05",months_selected))==1){
            load("clean/clean_tweets_2018_05_May.RData")
         }
         
        if (sum(grepl("2018-06",months_selected))==1){
            load("clean/clean_tweets_2018_06_June.RData")
         }
         
        if (sum(grepl("2018-07",months_selected))==1){
            load("clean/clean_tweets_2018_07_July.RData")
         }
         
        if (sum(grepl("2018-08",months_selected))==1){
            load("clean/clean_tweets_2018_08_August.RData")
         }
         
        if (sum(grepl("2018-09",months_selected))==1){
            load("clean/clean_tweets_2018_09_September.RData")
         }
         
        if (sum(grepl("2018-10",months_selected))==1){
            load("clean/clean_tweets_2018_10_October.RData")
         }
         
        if (sum(grepl("2018-11",months_selected))==1){
            load("clean/clean_tweets_2018_11_November.RData")
         }
         
        if (sum(grepl("2018-12",months_selected))==1){
            load("clean/clean_tweets_2018_12_December.RData")
         }
        
        ## 2019 ##
         
        if (sum(grepl("2019-01",months_selected))==1){
            load("clean/clean_tweets_2019_01_January.RData")
         }
         
        if (sum(grepl("2019-02",months_selected))==1){
            load("clean/clean_tweets_2019_02_February.RData")
         }
         
        if (sum(grepl("2019-03",months_selected))==1){
            load("clean/clean_tweets_2019_03_March.RData")
         }
         
        if (sum(grepl("2019-04",months_selected))==1){
            load("clean/clean_tweets_2019_04_April.RData")
         }
         
        if (sum(grepl("2019-05",months_selected))==1){
            load("clean/clean_tweets_2019_05_May.RData")
         }
         
        if (sum(grepl("2019-06",months_selected))==1){
            load("clean/clean_tweets_2019_06_June.RData")
         }
         
        if (sum(grepl("2019-07",months_selected))==1){
            load("clean/clean_tweets_2019_07_July.RData")
         }
         
        if (sum(grepl("2019-08",months_selected))==1){
            load("clean/clean_tweets_2019_08_August.RData")
         }
         
        if (sum(grepl("2019-09",months_selected))==1){
            load("clean/clean_tweets_2019_09_September.RData")
         }
         
        if (sum(grepl("2019-10",months_selected))==1){
            load("clean/clean_tweets_2019_10_October.RData")
         }
         
        if (sum(grepl("2019-11",months_selected))==1){
            load("clean/clean_tweets_2019_11_November.RData")
         }
         
        if (sum(grepl("2019-12",months_selected))==1){
            load("clean/clean_tweets_2019_12_December.RData")
         }
        
        ## 2020 ##
         
        if (sum(grepl("2020-01",months_selected))==1){
            load("clean/clean_tweets_2020_01_January.RData")
         }
         
        if (sum(grepl("2020-02",months_selected))==1){
            load("clean/clean_tweets_2020_02_February.RData")
         }
         
        if (sum(grepl("2020-03",months_selected))==1){
            load("clean/clean_tweets_2020_03_March.RData")
         }
         
        if (sum(grepl("2020-04",months_selected))==1){
            load("clean/clean_tweets_2020_04_April.RData")
         }
         
        if (sum(grepl("2020-05",months_selected))==1){
            load("clean/clean_tweets_2020_05_May.RData")
         }
         
        if (sum(grepl("2020-06",months_selected))==1){
            load("clean/clean_tweets_2020_06_June.RData")
         }
         
        if (sum(grepl("2020-07",months_selected))==1){
            load("clean/clean_tweets_2020_07_July.RData")
         }
         
        if (sum(grepl("2020-08",months_selected))==1){
            load("clean/clean_tweets_2020_08_August.RData")
         }
         
        if (sum(grepl("2020-09",months_selected))==1){
            load("clean/clean_tweets_2020_09_September.RData")
         }
         
        if (sum(grepl("2020-10",months_selected))==1){
            load("clean/clean_tweets_2020_10_October.RData")
         }
         
        if (sum(grepl("2020-11",months_selected))==1){
            load("clean/clean_tweets_2020_11_November.RData")
         }
         
        if (sum(grepl("2020-12",months_selected))==1){
            load("clean/clean_tweets_2020_12_December.RData")
         }
        
        
         
        if (sum(grepl("2021-01",months_selected))==1){
            load("clean/clean_tweets_2021_01_January.RData")
         }
         
        if (sum(grepl("2021-02",months_selected))==1){
            load("clean/clean_tweets_2021_02_February.RData")
         }
         
        if (sum(grepl("2021-03",months_selected))==1){
            load("clean/clean_tweets_2021_03_March.RData")
         }
         
        if (sum(grepl("2021-04",months_selected))==1){
            load("clean/clean_tweets_2021_04_April.RData")
         }
         
        if (sum(grepl("2021-05",months_selected))==1){
            load("clean/clean_tweets_2021_05_May.RData")
         }
         
        if (sum(grepl("2021-06",months_selected))==1){
            load("clean/clean_tweets_2021_06_June.RData")
         }
         
        if (sum(grepl("2021-07",months_selected))==1){
            load("clean/clean_tweets_2021_07_July.RData")
         }
         
        if (sum(grepl("2021-08",months_selected))==1){
            load("clean/clean_tweets_2021_08_August.RData")
         }
         
        if (sum(grepl("2021-09",months_selected))==1){
            load("clean/clean_tweets_2021_09_September.RData")
         }
         
        if (sum(grepl("2021-10",months_selected))==1){
            load("clean/clean_tweets_2021_10_October.RData")
         }
         
        if (sum(grepl("2021-11",months_selected))==1){
            load("clean/clean_tweets_2021_11_November.RData")
         }
         
        if (sum(grepl("2021-12",months_selected))==1){
            load("clean/clean_tweets_2021_12_December.RData")
         }
        
        
         
        if (sum(grepl("2022-01",months_selected))==1){
            load("clean/clean_tweets_2022_01_January.RData")
         }
        
        # load("MeToo_tweets/clean/clean_tweets_2021_11_November.RData")
        # load("MeToo_tweets/clean/clean_tweets_2021_12_December.RData")
        # load("MeToo_tweets/clean/clean_tweets_2022_01_January.RData")
        
        ls(pattern = "clean_tweets*")
        loaded_months <- ls(pattern = "clean_tweets*")
        df1 <- do.call("rbind", mget(loaded_months))
         # start_date = "2021-12-01"
         # end_date = "2022-01-31"
        
        #df1 <- df1[df1$date >= paste(as.character(start_date)) & df1$date <= paste(as.character(end_date)),]
        
        df1 <- df1[df1$date >= paste(as.character(start_date)),]
        df1 <- df1[df1$date <= paste(as.character(end_date)),]
        
        
        df1 <- df1[order(df1$created_at), ]
        
        # a1 <- make_a()
        # # b1 <- make_b()
        # # 
        # a <- as.numeric(a1)
        # # b <- as.numeric(b1)
        # 
        # df1 <- df1[sample(1:nrow(df1), a), ]
        # 
        #df1 <- df1[a:b,]
        df1 
        
        url1 <- rep("https://twitter.com/", length(df1$id))
        url2 <- rep("/status/", length(df1$id))
        
        
        df1$url <- paste(url1,df1$author_id,url2,df1$id,sep="")
        
        
        #url1 <- rep("https://twitter.com/anyuser/status/",length(df1$id))
        #df1$url <- paste(url1,df1$id,sep="")
        mft_boxes <- paste(mft_boxes2(), sep = " " , collapse = '')
        
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
            as.data.table(df1$public_metrics)
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
        
        list_df_fun <- function(){
            
            
            
            list.df = " "
            
            
            if (str_detect(mft_boxes, "author")) { 
                list.df=c(list.df,deparse(substitute(Author)))
            }
            
            if (str_detect(mft_boxes, "id")) { 
                list.df=c(list.df,deparse(substitute(ID)))
            }
            
            if (str_detect(mft_boxes, "link")) { 
                list.df=c(list.df,deparse(substitute(link)))
            }
            
            if (str_detect(mft_boxes, "text")) { 
                list.df=c(list.df,deparse(substitute(Content)))
            }
            
            if (str_detect(mft_boxes, "public_metrics")) { 
                list.df=c(list.df,deparse(substitute(Metrics)))
            }
            
            if (str_detect(mft_boxes, "created_at")) { 
                list.df=c(list.df,deparse(substitute(date_tag)))
            }
            
            if (str_detect(mft_boxes, "date_simple")) {
                list.df=c(list.df,deparse(substitute(date_simple)))
            }
            if (str_detect(mft_boxes, "lang")) {
                list.df=c(list.df,deparse(substitute(language)))
            }
            list.df
        }
        #mft_boxes <- "author"
        
        list.df <- list_df_fun()
        
        #list.df <- c(" ", deparse(substitute(df_a)),deparse(substitute(df_a)))
        
        list.df = list.df[2:length(list.df)] #remove first element 
        expression = paste0("df_combined = cbind(",paste0( list.df, collapse = ','), paste0(")"))
        
        eval(parse(text=expression))
        
        df_combined
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
