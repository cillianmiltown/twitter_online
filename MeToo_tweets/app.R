## app.R ##
library(shiny)
library(shinydashboard)

#library(shiny)
library("rdrop2")
library(tidyverse)
#library(shinydashboard)
library(academictwitteR)
library(data.table)

#### UI ####
ui <- dashboardPage(
    dashboardHeader(title = "#MeToo Tweets"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Raw Tweets",
                     tabName = "raw_tweets",
                     icon = icon("list", lib = "glyphicon")
            ),
            
            menuItem("Plot", tabName = "plot", icon = icon("signal", lib = "glyphicon")),
            menuItem("Clean Tweets (round 1)", tabName = "clean_tweets", icon = icon("list", lib = "glyphicon"))
        )
    ),
    dashboardBody(
        tabItems(
            ##### Raw Tweets (First tab) #####
            tabItem(tabName = "raw_tweets",
                    fluidRow(
                        box(
                            width = 3,
                            dateRangeInput('dateRange',
                                           label = 'Select Date Range:',
                                           start = "2022-01-01", end = "2022-01-31"
                            ),
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
                            ),
                            verbatimTextOutput("text_raw")
                            
                            
                        ),
                        
                        box(
                            
                            DT::dataTableOutput("table1"))
                    )
            ),
            
            ##### Plot (Second tab) #####
            tabItem(tabName = "plot",
                    fluidRow(
                        box(width = 3,
                            dateRangeInput('dateRange2',
                                           label = 'Select Date Range',
                                           start = "2017-09-01", end = Sys.Date()
                            ),
                            verbatimTextOutput("text_plot"),
                            checkboxInput("clean_plot", "Use Cleaned Data", FALSE
                            ),
                            
                            sliderInput("yAxisRange","Select Y-axis limit:"
                                        ,  min = 0, max = 200000, value = 200000
                                        #, step = 50000
                            )
                        ),
                        
                        box(
                            plotOutput("plot2")
                        )
                    )
            ),
            ##### Cleaned Tweets 1 (Third tab) #####
            tabItem(tabName = "clean_tweets",
                    fluidRow(
                        box(width = 3,
                            dateRangeInput('dateRange3',
                                           label = 'Select Date Range:',
                                           start = "2022-01-01", end = "2022-01-31"
                            ),
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
                            ),
                            
                            verbatimTextOutput("text_clean"),
                            radioButtons("duplicates", "Duplicated Tweets",
                                         choiceNames = c(
                                             "Do not Remove Duplicated Tweets",
                                             "Remove Duplicated Tweets",
                                             "Show only Duplicated Tweets"
                                         ),
                                         choiceValues = c(
                                             "keep_all"
                                             ,"remove_duplicated"       
                                             ,"show_duplicated") )
                        ),
                        box(DT::dataTableOutput("table2"))#,
                    )
            )
        )
    )
)

#### Server ####
server <- function(input, output) {
    
    ##### Create Reactives #####
    
    #all_tweets <- fread("all_tweets.csv")
    
    make_start_date <- reactive({input$dateRange[1]})
    make_end_date <- reactive({input$dateRange[2]})
    
    make_start_date2 <- reactive({input$dateRange2[1]})
    make_end_date2 <- reactive({input$dateRange2[2]})
    
    make_start_date3 <- reactive({input$dateRange3[1]})
    make_end_date3 <- reactive({input$dateRange3[2]})
    
    
    mft_boxes1 <- reactive({
        mft_boxes <- eval(parse(text = 'input$cols_select'))
        mft_boxes
    })
    
    mft_boxes2 <- reactive({
        mft_boxes <- eval(parse(text = 'input$cols_select2'))
        mft_boxes
    })
    
    duplicate_tweets_fun <- reactive({
        clean_box <- eval(parse(text = 'input$duplicates'))
        clean_box
    })
    
    
    plotrange = reactive({
        Num2 = as.numeric(as.character(input$yAxisRange))
        range = c(0, Num2)
        return(range)
    })
    
    ##### Tale of Raw Tweets ####
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
        
        df1 
        
        url1 <- rep("https://twitter.com/", length(df1$id))
        url2 <- rep("/status/", length(df1$id))
        
        
        df1$url <- paste(url1,df1$author_id,url2,df1$id,sep="")
        
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
        
        list.df = list.df[2:length(list.df)] #remove first element 
        expression = paste0("df_combined = cbind(",paste0( list.df, collapse = ','), paste0(")"))
        
        eval(parse(text=expression))
        
        df_combined
    })
    ##### Plot #####
    output$plot2<-renderPlot({
        
        if (
            input$clean_plot==1
        ) 
        {
            rm(y)
            load("clean/clean_data_for_plot.RData")
            y <- y_clean
        } else {
            rm(y)
            load("raw/data_for_plot.RData")
        }
        
        start_date <- make_start_date2()
        end_date <- make_end_date2()
        
        #y <- table(df1$date)
        
        y <- y[y$date >= paste(as.character(start_date)) & y$date <= paste(as.character(end_date)),]
        
        ggplot(y, aes(x=date, y=Freq
        )) +
            geom_col(position = "dodge",
                     color="black",
                     size=.2
            ) +
            coord_cartesian(ylim = plotrange() )+
            #xlab("Response to Critical Slide") +
            ylab("Total Daily Tweets")+
            scale_x_date(date_breaks="1 month",date_labels = "%B %Y ")+
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
    
    ##### Plot text #####
    output$text_plot <- renderText({
        
        if (
            input$clean_plot==1
        ) 
        {
            rm(y)
            load("clean/clean_data_for_plot.RData")
            y <- y_clean
        } else {
            rm(y)
            load("raw/data_for_plot.RData")
        }
        
        start_date <- make_start_date2()
        end_date <- make_end_date2()
        
        start_date <- make_start_date2()
        end_date <- make_end_date2()
        y <- y[y$date >= paste(as.character(start_date)) & y$date <= paste(as.character(end_date)),]
        
        text_to_display <- 
            paste("Total ", as.character(formatC(sum(y$Freq), big.mark=",")), " Tweets.")
        
        HTML(paste0((text_to_display)))
        
    })
    
    ##### raw text #####
    output$text_raw <- renderText({
        
        load("raw/data_for_plot.RData")
        
        start_date <- make_start_date2()
        end_date <- make_end_date2()
        
        start_date <- make_start_date2()
        end_date <- make_end_date2()
        y <- y[y$date >= paste(as.character(start_date)) & y$date <= paste(as.character(end_date)),]
        
        text_to_display <- 
            paste(as.character(formatC(sum(y$Freq), big.mark=",")), " Total")
        
        HTML(paste0((text_to_display)))
        
    })
    
    ##### clean text #####
    output$text_clean <- renderText({
        
        load("clean/clean_data_for_plot.RData")
        y <- y_clean
        
        
        start_date <- make_start_date2()
        end_date <- make_end_date2()
        
        start_date <- make_start_date2()
        end_date <- make_end_date2()
        y <- y[y$date >= paste(as.character(start_date)) & y$date <= paste(as.character(end_date)),]
        
        text_to_display <- 
            paste(as.character(formatC(sum(y$Freq), big.mark=",")), " Total")
        
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
    
    ##### Table of Clean Tweets #####
    output$table2 <- DT::renderDataTable({
        
        start_date <- make_start_date3()
        end_date <- make_end_date3()
        
        months_selected <- seq(as.Date(start_date), as.Date(end_date),by="month")
        
        
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
        
        
        ls(pattern = "clean_tweets*")
        loaded_months <- ls(pattern = "clean_tweets*")
        df1 <- do.call("rbind", mget(loaded_months))
        
        df1 <- df1[df1$date >= paste(as.character(start_date)),]
        df1 <- df1[df1$date <= paste(as.character(end_date)),]
        
        
        df1 <- df1[order(df1$created_at), ]
        df1 
        
        url1 <- rep("https://twitter.com/", length(df1$id))
        url2 <- rep("/status/", length(df1$id))
        
        
        df1$url <- paste(url1,df1$author_id,url2,df1$id,sep="")
        
        # "remove_duplicated"       
        # ,"show_duplicated"
        
        duplicate_tweets <- duplicate_tweets_fun()
        
        if (
            input$duplicates == "keep_all"
            #str_detect(clean_box, "clean_plot")
        ) 
        {
            df1 
        } else if (
            input$duplicates == "remove_duplicated"
        ) 
        {
            df1 <- df1[which(df1$num_dups==1),]
        } else if(
            input$duplicates == "show_duplicated"
        )
        {
            df1 <- df1[which(df1$num_dups>1),]
        } 
        
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

        list.df <- list_df_fun()
        

        list.df = list.df[2:length(list.df)] #remove first element 
        expression = paste0("df_combined = cbind(",paste0( list.df, collapse = ','), paste0(")"))
        
        eval(parse(text=expression))
        
        df_combined
    })
}

shinyApp(ui, server)