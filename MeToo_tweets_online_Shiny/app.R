## app.R ##
library(shiny)
#install.packages("shinydashboard")
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
                                           start = "2023-02-01", end = "2023-02-28"
                            ),
                            HTML("Please note: <br>",
                                 "1. Due to large volume of data selecting a narrow date range is recommended<br>",
                                 "2. Selecting a range across months can cause errors<br>",
                                 "&nbsp;"),
                            checkboxGroupInput("cols_select", "Select Columns",
                                               choiceNames = c(#"Author ID",
                                                               "Tweet ID",
                                                               "Link",
                                                               "Tweet Content",
                                                               "Metrics",
                                                               "Created",
                                                               "Date",
                                                               "Language"),
                                               choiceValues = c(
                                                   #"author"       
                                                   #,
                                                   "id"
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
                                           start = "2017-09-01", end = "2023-02-28"
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
                                           start = "2023-02-01", end = "2023-02-28"
                            ),
                            HTML("Please note: <br>",
                                 "1. Due to large volume of data selecting a narrow date range is recommended<br>",
                                 "2. Selecting a range across months can cause errors<br>",
                                 "&nbsp;"),
                            checkboxGroupInput("cols_select2", "Select Columns",
                                               choiceNames = c(#"Author ID",
                                                               "Tweet ID",
                                                               "Link",
                                                               "Tweet Content",
                                                               "Metrics",
                                                               "Created",
                                                               "Date",
                                                               "Language"),
                                               choiceValues = c(
                                                   #"author"       
                                                   #,
                                                 "id"
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
            load(url("https://osf.io/uwhde/download"))
        }
        if (grepl("2017-10",months_selected)){
            load(url("https://osf.io/8yh6w/download"))
        }
        if (grepl("2017-11",months_selected)){
            load(url("https://osf.io/e6sx7/download"))
        }
        if (grepl("2017-12",months_selected)){
            load(url("https://osf.io/rzmb8/download"))
        }
        
        ## 2018 ##
        if (grepl("2018-01",months_selected)){    
            load(url("https://osf.io/xz8h3/download"))
        }
        if (grepl("2018-02",months_selected)){
            load(url("https://osf.io/2dp89/download"))
        }
        if (grepl("2018-03",months_selected)){
            load(url("https://osf.io/v42c5/download"))
        }
        if (grepl("2018-04",months_selected)){
            load(url("https://osf.io/54cag/download"))
        }
        if (grepl("2018-05",months_selected)){
            load(url("https://osf.io/gvfm6/download"))
        }
        if (grepl("2018-06",months_selected)){
            load(url("https://osf.io/bpx9n/download"))
        }
        if (grepl("2018-07",months_selected)){
            load(url("https://osf.io/zav63/download"))
        }
        if (grepl("2018-08",months_selected)){
            load(url("https://osf.io/pxk8f/download"))
        }
        if (grepl("2018-09",months_selected)){
            load(url("https://osf.io/z689a/download"))
        }
        if (grepl("2018-10",months_selected)){
            load(url("https://osf.io/tc3kj/download"))
        }
        if (grepl("2018-11",months_selected)){
            load(url("https://osf.io/xscuz/download"))
        }
        if (grepl("2018-12",months_selected)){
            load(url("https://osf.io/68efp/download"))
        }
        
        ## 2019 ##
        if (grepl("2019-01",months_selected)){
            load(url("https://osf.io/9s6vj/download"))
        }
        if (grepl("2019-02",months_selected)){
            load(url("https://osf.io/49b7w/download"))
        }
        if (grepl("2019-03",months_selected)){
            load(url("https://osf.io/7ufv9/download"))
        }
        if (grepl("2019-04",months_selected)){
            load(url("https://osf.io/8d9cx/download"))
        }
        if (grepl("2019-05",months_selected)){
            load(url("https://osf.io/b5xqj/download"))
        }
        if (grepl("2019-06",months_selected)){
            load(url("https://osf.io/g72np/download"))
        }
        if (grepl("2019-07",months_selected)){
            load(url("https://osf.io/fntbv/download"))
        }
        if (grepl("2019-08",months_selected)){
            load(url("https://osf.io/dvf7b/download"))
        }
        if (grepl("2019-09",months_selected)){
            load(url("https://osf.io/pjrdw/download"))
        }
        if (grepl("2019-10",months_selected)){
            load(url("https://osf.io/v6xet/download"))
        }
        if (grepl("2019-11",months_selected)){
            load(url("https://osf.io/56adb/download"))
        }
        if (grepl("2019-12",months_selected)){
            load(url("https://osf.io/k486z/download"))
        }
        
        ## 2020 ##
        if (grepl("2020-01",months_selected)){
            load(url("https://osf.io/r9v8y/download"))
        }
        if (grepl("2020-02",months_selected)){
            load(url("https://osf.io/u638b/download"))
        }
        if (grepl("2020-03",months_selected)){
            load(url("https://osf.io/4h6u3/download"))
        }
        if (grepl("2020-04",months_selected)){
            load(url("https://osf.io/kxvrd/download"))
        }
        if (grepl("2020-05",months_selected)){
            load(url("https://osf.io/9jxps/download"))
        }
        if (grepl("2020-06",months_selected)){
            load(url("https://osf.io/26hkj/download"))
        }
        if (grepl("2020-07",months_selected)){
            load(url("https://osf.io/djhs3/download"))
        }
        if (grepl("2020-08",months_selected)){
            load(url("https://osf.io/bvkwq/download"))
        }
        if (grepl("2020-09",months_selected)){
            load(url("https://osf.io/msbeh/download"))
        }
        if (grepl("2020-10",months_selected)){
            load(url("https://osf.io/qrw74/download"))
        }
        if (grepl("2020-11",months_selected)){
            load(url("https://osf.io/ns2xk/download"))
        }
        if (grepl("2020-12",months_selected)){
            load(url("https://osf.io/8ry59/download"))
        }
        
        
        
        if (grepl("2021-01",months_selected)){
            load(url("https://osf.io/gquxb/download"))
        }
        if (grepl("2021-02",months_selected)){
            load(url("https://osf.io/pjdxk/download"))
        }
        if (grepl("2021-03",months_selected)){
            load(url("https://osf.io/f4za3/download"))
        }
        if (grepl("2021-04",months_selected)){
            load(url("https://osf.io/p7g6w/download"))
        }
        if (grepl("2021-05",months_selected)){
            load(url("https://osf.io/uxyvt/download"))
        }
        if (grepl("2021-06",months_selected)){
            load(url("https://osf.io/2n6zv/download"))
        }
        if (grepl("2021-07",months_selected)){
            load(url("https://osf.io/hb5nx/download"))
        }
        if (grepl("2021-08",months_selected)){
            load(url("https://osf.io/dwgp9/download"))
        }
        if (grepl("2021-09",months_selected)){
            load(url("https://osf.io/5zty4/download"))
        }
        if (grepl("2021-10",months_selected)){
            load(url("https://osf.io/s28w5/download"))
        }
        if (grepl("2021-11",months_selected)){
            load(url("https://osf.io/756pt/download"))
        }
        if (grepl("2021-12",months_selected)){
            load(url("https://osf.io/amjqk/download"))
        }
        
        
        if (grepl("2022-01",months_selected)){
            load(url("https://osf.io/9ebmf/download"))
        }
        if (grepl("2022-02",months_selected)){
            load(url("https://osf.io/yxcm5/download"))
        }
        if (grepl("2022-03",months_selected)){
            load(url("https://osf.io/kdjs7/download"))
        }
        if (grepl("2022-04",months_selected)){
            load(url("https://osf.io/yqd4b/download"))
        }
        if (grepl("2022-05",months_selected)){
            load(url("https://osf.io/rh2ga/download"))
        }
        if (grepl("2022-06",months_selected)){
          load(url("https://osf.io/mvbr7/download"))
        }
        if (grepl("2022-07",months_selected)){
          load(url("https://osf.io/gkz9t/download"))
        }
        if (grepl("2022-08",months_selected)){
          load(url("https://osf.io/eqtu4/download"))
        }
        if (grepl("2022-09",months_selected)){
          load(url("https://osf.io/9t6uq/download"))
        }
        if (grepl("2022-10",months_selected)){
          load(url("https://osf.io/52exq/download"))
        }
        if (grepl("2022-11",months_selected)){
          load(url("https://osf.io/gfba5/download"))
        }
        if (grepl("2022-12",months_selected)){
          load(url("https://osf.io/5k3du/download"))
        }
        
        
        
        if (grepl("2023-01",months_selected)){
          load(url("https://osf.io/2e79r/download"))
        }
        if (grepl("2023-02",months_selected)){
          load(url("https://osf.io/2g3rm/download"))
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
            load(url("https://osf.io/8kt4x/download"))
            y <- y_clean
        } else {
            rm(y)
            load(url("https://osf.io/6tme5/download"))
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
            load(url("https://osf.io/8kt4x/download"))
            y <- y_clean
        } else {
            rm(y)
            load(url("https://osf.io/6tme5/download"))
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
        
        load(url("https://osf.io/6tme5/download"))
        
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
        
        load(url("https://osf.io/8kt4x/download"))
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
            load(url("https://osf.io/8pjd9/download"))
        }
        
        if (sum(grepl("2017-10",months_selected))==1){
          load(url("https://osf.io/d8tny/download"))
        }
        
        if (sum(grepl("2017-11",months_selected))==1){
            load(url("https://osf.io/a4py8/download"))
        }
        
        if (sum(grepl("2017-12",months_selected))==1){
            load(url("https://osf.io/xj4n9/download"))
        }
        
        ## 2018 ##
        
        if (sum(grepl("2018-01",months_selected))==1){    
            load(url("https://osf.io/s9wht/download"))
        }
        
        if (sum(grepl("2018-02",months_selected))==1){
            load(url("https://osf.io/nhb3w/download"))
        }
        
        if (sum(grepl("2018-03",months_selected))==1){
            load(url("https://osf.io/gxfpr/download"))
        }
        
        if (sum(grepl("2018-04",months_selected))==1){
            load(url("https://osf.io/psgmu/download"))
        }
        
        if (sum(grepl("2018-05",months_selected))==1){
            load(url("https://osf.io/8ukpy/download"))
        }
        
        if (sum(grepl("2018-06",months_selected))==1){
            load(url("https://osf.io/ug9db/download"))
        }
        
        if (sum(grepl("2018-07",months_selected))==1){
            load(url("https://osf.io/57bz2/download"))
        }
        
        if (sum(grepl("2018-08",months_selected))==1){
            load(url("https://osf.io/tw5ax/download"))
        }
        
        if (sum(grepl("2018-09",months_selected))==1){
            load(url("https://osf.io/bdjz3/download"))
        }
        
        if (sum(grepl("2018-10",months_selected))==1){
            load(url("https://osf.io/jq2hg/download"))
        }
        
        if (sum(grepl("2018-11",months_selected))==1){
            load(url("https://osf.io/5hdkc/download"))
        }
        
        if (sum(grepl("2018-12",months_selected))==1){
            load(url("https://osf.io/zcuae/download"))
        }
        
        ## 2019 ##
        
        if (sum(grepl("2019-01",months_selected))==1){
            load(url("https://osf.io/zcuae/download"))
        }
        
        if (sum(grepl("2019-02",months_selected))==1){
            load(url("https://osf.io/ysh35/download"))
        }
        
        if (sum(grepl("2019-03",months_selected))==1){
            load(url("https://osf.io/khea7/download"))
        }
        
        if (sum(grepl("2019-04",months_selected))==1){
            load(url("https://osf.io/5yngz/download"))
        }
        
        if (sum(grepl("2019-05",months_selected))==1){
            load(url("https://osf.io/rgq2c/download"))
        }
        
        if (sum(grepl("2019-06",months_selected))==1){
            load(url("https://osf.io/jgwpv/download"))
        }
        
        if (sum(grepl("2019-07",months_selected))==1){
            load(url("https://osf.io/2bdpe/download"))
        }
        
        if (sum(grepl("2019-08",months_selected))==1){
            load(url("https://osf.io/gt25a/download"))
        }
        
        if (sum(grepl("2019-09",months_selected))==1){
            load(url("https://osf.io/487tb/download"))
        }
        
        if (sum(grepl("2019-10",months_selected))==1){
            load(url("https://osf.io/pt9s4/download"))
        }
        
        if (sum(grepl("2019-11",months_selected))==1){
            load(url("https://osf.io/2zm4j/download"))
        }
        
        if (sum(grepl("2019-12",months_selected))==1){
            load(url("https://osf.io/6vt5p/download"))
        }
        
        ## 2020 ##
        
        if (sum(grepl("2020-01",months_selected))==1){
            load(url("https://osf.io/7rp25/download"))
        }
        
        if (sum(grepl("2020-02",months_selected))==1){
            load(url("https://osf.io/epsau/download"))
        }
        
        if (sum(grepl("2020-03",months_selected))==1){
            load(url("https://osf.io/hr4ue/download"))
        }
        
        if (sum(grepl("2020-04",months_selected))==1){
            load(url("https://osf.io/jchqs/download"))
        }
        
        if (sum(grepl("2020-05",months_selected))==1){
            load(url("https://osf.io/ev9gd/download"))
        }
        
        if (sum(grepl("2020-06",months_selected))==1){
            load(url("https://osf.io/rpt35/download"))
        }
        
        if (sum(grepl("2020-07",months_selected))==1){
            load(url("https://osf.io/vgwzk/download"))
        }
        
        if (sum(grepl("2020-08",months_selected))==1){
            load(url("https://osf.io/pcqta/download"))
        }
        
        if (sum(grepl("2020-09",months_selected))==1){
            load(url("https://osf.io/w837s/download"))
        }
        
        if (sum(grepl("2020-10",months_selected))==1){
            load(url("https://osf.io/k2csz/download"))
        }
        
        if (sum(grepl("2020-11",months_selected))==1){
            load(url("https://osf.io/vme82/download"))
        }
        
        if (sum(grepl("2020-12",months_selected))==1){
            load(url("https://osf.io/r6jp4/download"))
        }
        
        
        
        if (sum(grepl("2021-01",months_selected))==1){
            load(url("https://osf.io/24hfe/download"))
        }
        
        if (sum(grepl("2021-02",months_selected))==1){
            load(url("https://osf.io/xtcgv/download"))
        }
        
        if (sum(grepl("2021-03",months_selected))==1){
            load(url("https://osf.io/y9bhv/download"))
        }
        
        if (sum(grepl("2021-04",months_selected))==1){
            load(url("https://osf.io/k3zj4/download"))
        }
        
        if (sum(grepl("2021-05",months_selected))==1){
            load(url("https://osf.io/v3dmy/download"))
        }
        
        if (sum(grepl("2021-06",months_selected))==1){
            load(url("https://osf.io/zjb36/download"))
        }
        
        if (sum(grepl("2021-07",months_selected))==1){
            load(url("https://osf.io/rv65f/download"))
        }
        
        if (sum(grepl("2021-08",months_selected))==1){
            load(url("https://osf.io/hd94a/download"))
        }
        
        if (sum(grepl("2021-09",months_selected))==1){
            load(url("https://osf.io/mk8pv/download"))
        }
        
        if (sum(grepl("2021-10",months_selected))==1){
            load(url("https://osf.io/pdh7b/download"))
        }
        
        if (sum(grepl("2021-11",months_selected))==1){
            load(url("https://osf.io/n7zfw/download"))
        }
        
        if (sum(grepl("2021-12",months_selected))==1){
            load(url("https://osf.io/a7zxy/download"))
        }
        
        
        
        if (sum(grepl("2022-01",months_selected))==1){
            load(url("https://osf.io/5qywm/download"))
        }
        if (sum(grepl("2022-02",months_selected))==1){
            load(url("https://osf.io/ka3e7/download"))
        }
        if (sum(grepl("2022-03",months_selected))==1){
            load(url("https://osf.io/f7me2/download"))
        }
        if (sum(grepl("2022-04",months_selected))==1){
            load(url("https://osf.io/n3u5t/download"))
        }
        if (sum(grepl("2022-05",months_selected))==1){
            load(url("https://osf.io/npjyv/download"))
        }
        if (grepl("2022-06",months_selected)){
          load(url("https://osf.io/5m89c/download"))
        }
        if (grepl("2022-07",months_selected)){
          load(url("https://osf.io/25k3r/download"))
        }
        if (grepl("2022-08",months_selected)){
          load(url("https://osf.io/z7dwv/download"))
        }
        if (grepl("2022-09",months_selected)){
          load(url("https://osf.io/w7hqz/download"))
        }
        if (grepl("2022-10",months_selected)){
          load(url("https://osf.io/2fh4w/download"))
        }
        if (grepl("2022-11",months_selected)){
          load(url("https://osf.io/uj3q5/download"))
        }
        if (grepl("2022-12",months_selected)){
          load(url("https://osf.io/eszp5/download"))
        }
        
        
        
        if (grepl("2023-01",months_selected)){
          load(url("https://osf.io/2g6zc/download"))
        }
        if (grepl("2023-02",months_selected)){
          load(url("https://osf.io/px2n3/download"))
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