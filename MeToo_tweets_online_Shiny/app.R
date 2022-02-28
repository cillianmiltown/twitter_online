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
            load(url("https://www.dropbox.com/s/gdbmdy2jjrzci5o/2017_09_September.RData?dl=1"))
        }
        if (grepl("2017-10",months_selected)){
            load(url("https://www.dropbox.com/s/a16weqp01odm5ge/2017_10_October.RData?dl=1"))
        }
        if (grepl("2017-11",months_selected)){
            load(url("https://www.dropbox.com/s/f0sakjqcr0zn8hk/2017_11_November.RData?dl=1"))
        }
        if (grepl("2017-12",months_selected)){
            load(url("https://www.dropbox.com/s/yet9s1mfpqmoh4l/2017_12_December.RData?dl=1"))
        }
        
        ## 2018 ##
        if (grepl("2018-01",months_selected)){    
            load(url("https://www.dropbox.com/s/5f5ofj2jrh18rjn/2018_01_January.RData?dl=1"))
        }
        if (grepl("2018-02",months_selected)){
            load(url("https://www.dropbox.com/s/2zjdqljkwt7pr91/2018_02_February.RData?dl=1"))
        }
        if (grepl("2018-03",months_selected)){
            load(url("https://www.dropbox.com/s/cs0uardtxltv4af/2018_03_March.RData?dl=1"))
        }
        if (grepl("2018-04",months_selected)){
            load(url("https://www.dropbox.com/s/5vplmozyhdj8fge/2018_04_April.RData?dl=1"))
        }
        if (grepl("2018-05",months_selected)){
            load(url("https://www.dropbox.com/s/ueonivxpmairlrz/2018_05_May.RData?dl=1"))
        }
        if (grepl("2018-06",months_selected)){
            load(url("https://www.dropbox.com/s/7hc6lihhy5e57ex/2018_06_June.RData?dl=1"))
        }
        if (grepl("2018-07",months_selected)){
            load(url("https://www.dropbox.com/s/ob2vcxtli6r6rtb/2018_07_July.RData?dl=1"))
        }
        if (grepl("2018-08",months_selected)){
            load(url("https://www.dropbox.com/s/jeerhose0giog2l/2018_08_August.RData?dl=1"))
        }
        if (grepl("2018-09",months_selected)){
            load(url("https://www.dropbox.com/s/lxce7va77ktzk0b/2018_09_September.RData?dl=1"))
        }
        if (grepl("2018-10",months_selected)){
            load(url("https://www.dropbox.com/s/qe5jnyl3nia7yqn/2018_10_October.RData?dl=1"))
        }
        if (grepl("2018-11",months_selected)){
            load(url("https://www.dropbox.com/s/77mtttrhck3tow2/2018_11_November.RData?dl=1"))
        }
        if (grepl("2018-12",months_selected)){
            load(url("https://www.dropbox.com/s/5jt9xqub6xbvxjx/2018_12_December.RData?dl=1"))
        }
        
        ## 2019 ##
        if (grepl("2019-01",months_selected)){
            load(url("https://www.dropbox.com/s/56mkvmn27b7r912/2019_01_January.RData?dl=1"))
        }
        if (grepl("2019-02",months_selected)){
            load(url("https://www.dropbox.com/s/hjpbgbupgogk52i/2019_02_February.RData?dl=1"))
        }
        if (grepl("2019-03",months_selected)){
            load(url("https://www.dropbox.com/s/u2fk8phcpx5b1qy/2019_03_March.RData?dl=1"))
        }
        if (grepl("2019-04",months_selected)){
            load(url("https://www.dropbox.com/s/sr1ldyvvfravvwx/2019_04_April.RData?dl=1"))
        }
        if (grepl("2019-05",months_selected)){
            load(url("https://www.dropbox.com/s/td40bvmzhf53i1j/2019_05_May.RData?dl=1"))
        }
        if (grepl("2019-06",months_selected)){
            load(url("https://www.dropbox.com/s/m2rtkl2vf8v8qet/2019_06_June.RData?dl=1"))
        }
        if (grepl("2019-07",months_selected)){
            load(url("https://www.dropbox.com/s/53vnonfcs3p7y0l/2019_07_July.RData?dl=1"))
        }
        if (grepl("2019-08",months_selected)){
            load(url("https://www.dropbox.com/s/l6bc10xorcfi3ml/2019_08_August.RData?dl=1"))
        }
        if (grepl("2019-09",months_selected)){
            load(url("https://www.dropbox.com/s/cswazvvxzbnwufo/2019_09_September.RData?dl=1"))
        }
        if (grepl("2019-10",months_selected)){
            load(url("https://www.dropbox.com/s/iqnxo6pmvqsgebt/2019_10_October.RData?dl=1"))
        }
        if (grepl("2019-11",months_selected)){
            load(url("https://www.dropbox.com/s/lbo6aa19ftmiozv/2019_11_November.RData?dl=1"))
        }
        if (grepl("2019-12",months_selected)){
            load(url("https://www.dropbox.com/s/thsok1vgjd2scrw/2019_12_December.RData?dl=1"))
        }
        
        ## 2020 ##
        if (grepl("2020-01",months_selected)){
            load(url("https://www.dropbox.com/s/bcjeeg3pdh20aiu/2020_01_January.RData?dl=1"))
        }
        if (grepl("2020-02",months_selected)){
            load(url("https://www.dropbox.com/s/6i6wp4i1xjk89r5/2020_02_February.RData?dl=1"))
        }
        if (grepl("2020-03",months_selected)){
            load(url("https://www.dropbox.com/s/axwf0d0fscq5uy3/2020_03_March.RData?dl=1"))
        }
        if (grepl("2020-04",months_selected)){
            load(url("https://www.dropbox.com/s/7qbvu1odoflk3ws/2020_04_April.RData?dl=1"))
        }
        if (grepl("2020-05",months_selected)){
            load(url("https://www.dropbox.com/s/gt5u0kqlk3fz2ou/2020_05_May.RData?dl=1"))
        }
        if (grepl("2020-06",months_selected)){
            load(url("https://www.dropbox.com/s/evdoc0keab80hy3/2020_06_June.RData?dl=1"))
        }
        if (grepl("2020-07",months_selected)){
            load(url("https://www.dropbox.com/s/1oy95z7g4vebkom/2020_07_July.RData?dl=1"))
        }
        if (grepl("2020-08",months_selected)){
            load(url("https://www.dropbox.com/s/ks09weqeir5l30j/2020_08_August.RData?dl=1"))
        }
        if (grepl("2020-09",months_selected)){
            load(url("https://www.dropbox.com/s/iidh15pa8o46yh3/2020_09_September.RData?dl=1"))
        }
        if (grepl("2020-10",months_selected)){
            load(url("https://www.dropbox.com/s/5azswxj8gxwjfhk/2020_10_October.RData?dl=1"))
        }
        if (grepl("2020-11",months_selected)){
            load(url("https://www.dropbox.com/s/ja9h5ydniy1k2dn/2020_11_November.RData?dl=1"))
        }
        if (grepl("2020-12",months_selected)){
            load(url("https://www.dropbox.com/s/1oj1iomtu4rpeov/2020_12_December.RData?dl=1"))
        }
        
        
        
        if (grepl("2021-01",months_selected)){
            load(url("https://www.dropbox.com/s/6jlrhaflygznnta/2021_01_January.RData?dl=1"))
        }
        if (grepl("2021-02",months_selected)){
            load(url("https://www.dropbox.com/s/shoypto4xorrq55/2021_02_February.RData?dl=1"))
        }
        if (grepl("2021-03",months_selected)){
            load(url("https://www.dropbox.com/s/vu9bvf69xhluq2l/2021_03_March.RData?dl=1"))
        }
        if (grepl("2021-04",months_selected)){
            load(url("https://www.dropbox.com/s/luxe3cm4twvpa63/2021_04_April.RData?dl=1"))
        }
        if (grepl("2021-05",months_selected)){
            load(url("https://www.dropbox.com/s/8hulzoi82xtjavw/2021_05_May.RData?dl=1"))
        }
        if (grepl("2021-06",months_selected)){
            load(url("https://www.dropbox.com/s/jiq1vobgfbgb1u2/2021_06_June.RData?dl=1"))
        }
        if (grepl("2021-07",months_selected)){
            load(url("https://www.dropbox.com/s/dx49kd4s77cgcn0/2021_07_July.RData?dl=1"))
        }
        if (grepl("2021-08",months_selected)){
            load(url("https://www.dropbox.com/s/u9z7otbqwkftogm/2021_08_August.RData?dl=1"))
        }
        if (grepl("2021-09",months_selected)){
            load(url("https://www.dropbox.com/s/abjf2lqdp5ndoth/2021_09_September.RData?dl=1"))
        }
        if (grepl("2021-10",months_selected)){
            load(url("https://www.dropbox.com/s/67273fq7k5qbc0a/2021_10_October.RData?dl=1"))
        }
        if (grepl("2021-11",months_selected)){
            load(url("https://www.dropbox.com/s/jjpa1l7g9kixv04/2021_11_November.RData?dl=1"))
        }
        if (grepl("2021-12",months_selected)){
            load(url("https://www.dropbox.com/s/kza2rj4do6zu484/2021_12_December.RData?dl=1"))
        }
        
        
        if (grepl("2022-01",months_selected)){
            load(url("https://www.dropbox.com/s/w4v0h5qqkbpplxv/2022_01_January.RData?dl=1"))
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
            load(url("https://www.dropbox.com/s/jvo6zk6ciphowbc/clean_data_for_plot.RData?dl=1"))
            y <- y_clean
        } else {
            rm(y)
            load(url("https://www.dropbox.com/s/jaa8klpmvdxu9sr/data_for_plot.RData?dl=1"))
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
            load(url("https://www.dropbox.com/s/jvo6zk6ciphowbc/clean_data_for_plot.RData?dl=1"))
            y <- y_clean
        } else {
            rm(y)
            load(url("https://www.dropbox.com/s/jaa8klpmvdxu9sr/data_for_plot.RData?dl=1"))
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
        
        load(url("https://www.dropbox.com/s/jaa8klpmvdxu9sr/data_for_plot.RData?dl=1"))
        
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
        
        load(url("https://www.dropbox.com/s/jvo6zk6ciphowbc/clean_data_for_plot.RData?dl=1"))
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
            load(url("https://www.dropbox.com/s/s3t1rqfic21s3pu/clean_tweets_2017_09_September.RData?dl=1"))
        }
        
        if (sum(grepl("2017-10",months_selected))==1){
            load(url("https://www.dropbox.com/s/air9awpefrl0xvb/clean_tweets_2017_10_October.RData?dl=1"))
        }
        
        if (sum(grepl("2017-11",months_selected))==1){
            load(url("https://www.dropbox.com/s/9dhftbmmz0duq3v/clean_tweets_2017_11_November.RData?dl=1"))
        }
        
        if (sum(grepl("2017-12",months_selected))==1){
            load(url("https://www.dropbox.com/s/f2wt5c9upkjeeiq/clean_tweets_2017_12_December.RData?dl=1"))
        }
        
        ## 2018 ##
        
        if (sum(grepl("2018-01",months_selected))==1){    
            load(url("https://www.dropbox.com/s/auhl4l541wgtzif/clean_tweets_2018_01_January.RData?dl=1"))
        }
        
        if (sum(grepl("2018-02",months_selected))==1){
            load(url("https://www.dropbox.com/s/y2gaqkuely0u5jp/clean_tweets_2018_02_February.RData?dl=1"))
        }
        
        if (sum(grepl("2018-03",months_selected))==1){
            load(url("https://www.dropbox.com/s/tn3srq6wki9x4gk/clean_tweets_2018_03_March.RData?dl=1"))
        }
        
        if (sum(grepl("2018-04",months_selected))==1){
            load(url("https://www.dropbox.com/s/boki6bakfevjpr2/clean_tweets_2018_04_April.RData?dl=1"))
        }
        
        if (sum(grepl("2018-05",months_selected))==1){
            load(url("https://www.dropbox.com/s/a1r1d782qqe1dpc/clean_tweets_2018_05_May.RData?dl=1"))
        }
        
        if (sum(grepl("2018-06",months_selected))==1){
            load(url("https://www.dropbox.com/s/350ho3fqtgonxpx/clean_tweets_2018_06_June.RData?dl=1"))
        }
        
        if (sum(grepl("2018-07",months_selected))==1){
            load(url("https://www.dropbox.com/s/25vnr3jw1nzf54y/clean_tweets_2018_07_July.RData?dl=1"))
        }
        
        if (sum(grepl("2018-08",months_selected))==1){
            load(url("https://www.dropbox.com/s/j19ohi4mjca1eqt/clean_tweets_2018_08_August.RData?dl=1"))
        }
        
        if (sum(grepl("2018-09",months_selected))==1){
            load(url("https://www.dropbox.com/s/8558wjgmybzubsh/clean_tweets_2018_09_September.RData?dl=1"))
        }
        
        if (sum(grepl("2018-10",months_selected))==1){
            load(url("https://www.dropbox.com/s/psh6kqpsouoewwf/clean_tweets_2018_10_October.RData?dl=1"))
        }
        
        if (sum(grepl("2018-11",months_selected))==1){
            load(url("https://www.dropbox.com/s/1anoe5n6s5rhtq1/clean_tweets_2018_11_November.RData?dl=1"))
        }
        
        if (sum(grepl("2018-12",months_selected))==1){
            load(url("https://www.dropbox.com/s/vjj9i2yuj3sual1/clean_tweets_2018_12_December.RData?dl=1"))
        }
        
        ## 2019 ##
        
        if (sum(grepl("2019-01",months_selected))==1){
            load(url("https://www.dropbox.com/s/3i8s53ydqvv3yox/clean_tweets_2019_01_January.RData?dl=1"))
        }
        
        if (sum(grepl("2019-02",months_selected))==1){
            load(url("https://www.dropbox.com/s/yvokg1vu3123eoh/clean_tweets_2019_02_February.RData?dl=1"))
        }
        
        if (sum(grepl("2019-03",months_selected))==1){
            load(url("https://www.dropbox.com/s/ccl0ziv2r5ut89d/clean_tweets_2019_03_March.RData?dl=1"))
        }
        
        if (sum(grepl("2019-04",months_selected))==1){
            load(url("https://www.dropbox.com/s/2fyaxgg27177xrk/clean_tweets_2019_04_April.RData?dl=1"))
        }
        
        if (sum(grepl("2019-05",months_selected))==1){
            load(url("https://www.dropbox.com/s/d2ih0b2tbr37ucq/clean_tweets_2019_05_May.RData?dl=1"))
        }
        
        if (sum(grepl("2019-06",months_selected))==1){
            load(url("https://www.dropbox.com/s/fmhn2vn50ov6q48/clean_tweets_2019_06_June.RData?dl=1"))
        }
        
        if (sum(grepl("2019-07",months_selected))==1){
            load(url("https://www.dropbox.com/s/1txz433rg087dvi/clean_tweets_2019_07_July.RData?dl=1"))
        }
        
        if (sum(grepl("2019-08",months_selected))==1){
            load(url("https://www.dropbox.com/s/gyv6pnuvdufa9dp/clean_tweets_2019_08_August.RData?dl=1"))
        }
        
        if (sum(grepl("2019-09",months_selected))==1){
            load(url("https://www.dropbox.com/s/6zvyyw47fi4z9xz/clean_tweets_2019_09_September.RData?dl=1"))
        }
        
        if (sum(grepl("2019-10",months_selected))==1){
            load(url("https://www.dropbox.com/s/1z7w1hhoj1n2vg3/clean_tweets_2019_10_October.RData?dl=1"))
        }
        
        if (sum(grepl("2019-11",months_selected))==1){
            load(url("https://www.dropbox.com/s/et8y9i40kgs0thg/clean_tweets_2019_11_November.RData?dl=1"))
        }
        
        if (sum(grepl("2019-12",months_selected))==1){
            load(url("https://www.dropbox.com/s/tp2jr36v292ao03/clean_tweets_2019_12_December.RData?dl=1"))
        }
        
        ## 2020 ##
        
        if (sum(grepl("2020-01",months_selected))==1){
            load(url("https://www.dropbox.com/s/om5en1d3j5d6nso/clean_tweets_2020_01_January.RData?dl=1"))
        }
        
        if (sum(grepl("2020-02",months_selected))==1){
            load(url("https://www.dropbox.com/s/ckt3adixbarns5x/clean_tweets_2020_02_February.RData?dl=1"))
        }
        
        if (sum(grepl("2020-03",months_selected))==1){
            load(url("https://www.dropbox.com/s/def446g79xjd3rv/clean_tweets_2020_03_March.RData?dl=1"))
        }
        
        if (sum(grepl("2020-04",months_selected))==1){
            load(url("https://www.dropbox.com/s/ofe9un3qm53q2tn/clean_tweets_2020_04_April.RData?dl=1"))
        }
        
        if (sum(grepl("2020-05",months_selected))==1){
            load(url("https://www.dropbox.com/s/n4ixyl5a9qz7bp2/clean_tweets_2020_05_May.RData?dl=1"))
        }
        
        if (sum(grepl("2020-06",months_selected))==1){
            load(url("https://www.dropbox.com/s/yvsc3negkm7f6ai/clean_tweets_2020_06_June.RData?dl=1"))
        }
        
        if (sum(grepl("2020-07",months_selected))==1){
            load(url("https://www.dropbox.com/s/r91v2ucpp9e9qqn/clean_tweets_2020_07_July.RData?dl=1"))
        }
        
        if (sum(grepl("2020-08",months_selected))==1){
            load(url("https://www.dropbox.com/s/a34fy1u9uot55l3/clean_tweets_2020_08_August.RData?dl=1"))
        }
        
        if (sum(grepl("2020-09",months_selected))==1){
            load(url("https://www.dropbox.com/s/uuqyfklfs384flk/clean_tweets_2020_09_September.RData?dl=1"))
        }
        
        if (sum(grepl("2020-10",months_selected))==1){
            load(url("https://www.dropbox.com/s/hdt0ewgug9lwj1l/clean_tweets_2020_10_October.RData?dl=1"))
        }
        
        if (sum(grepl("2020-11",months_selected))==1){
            load(url("https://www.dropbox.com/s/uh3bwstaqnjgi38/clean_tweets_2020_11_November.RData?dl=1"))
        }
        
        if (sum(grepl("2020-12",months_selected))==1){
            load(url("https://www.dropbox.com/s/ms6wd3koa4ww59n/clean_tweets_2020_12_December.RData?dl=1"))
        }
        
        
        
        if (sum(grepl("2021-01",months_selected))==1){
            load(url("https://www.dropbox.com/s/ymubezrrtk548v2/clean_tweets_2021_01_January.RData?dl=1"))
        }
        
        if (sum(grepl("2021-02",months_selected))==1){
            load(url("https://www.dropbox.com/s/3ib4lsnzxuoycxd/clean_tweets_2021_02_February.RData?dl=1"))
        }
        
        if (sum(grepl("2021-03",months_selected))==1){
            load(url("https://www.dropbox.com/s/ci84ygcrka157zi/clean_tweets_2021_03_March.RData?dl=1"))
        }
        
        if (sum(grepl("2021-04",months_selected))==1){
            load(url("https://www.dropbox.com/s/imkrr51aqq9piil/clean_tweets_2021_04_April.RData?dl=1"))
        }
        
        if (sum(grepl("2021-05",months_selected))==1){
            load(url("https://www.dropbox.com/s/mjo251irnoe966n/clean_tweets_2021_05_May.RData?dl=1"))
        }
        
        if (sum(grepl("2021-06",months_selected))==1){
            load(url("https://www.dropbox.com/s/hkf0aymbyqe9khj/clean_tweets_2021_06_June.RData?dl=1"))
        }
        
        if (sum(grepl("2021-07",months_selected))==1){
            load(url("https://www.dropbox.com/s/yu33xh1inva648x/clean_tweets_2021_07_July.RData?dl=1"))
        }
        
        if (sum(grepl("2021-08",months_selected))==1){
            load(url("https://www.dropbox.com/s/wq7pid0tsyqn7em/clean_tweets_2021_08_August.RData?dl=1"))
        }
        
        if (sum(grepl("2021-09",months_selected))==1){
            load(url("https://www.dropbox.com/s/oz02u03srgvk0sm/clean_tweets_2021_09_September.RData?dl=1"))
        }
        
        if (sum(grepl("2021-10",months_selected))==1){
            load(url("https://www.dropbox.com/s/qdibe9e42owpioq/clean_tweets_2021_10_October.RData?dl=1"))
        }
        
        if (sum(grepl("2021-11",months_selected))==1){
            load(url("https://www.dropbox.com/s/vs5lozemibgvgsm/clean_tweets_2021_11_November.RData?dl=1"))
        }
        
        if (sum(grepl("2021-12",months_selected))==1){
            load(url("https://www.dropbox.com/s/2y7to3zozu3h3h9/clean_tweets_2021_12_December.RData?dl=1"))
        }
        
        
        
        if (sum(grepl("2022-01",months_selected))==1){
            load(url("https://www.dropbox.com/s/dcparb3wdbh66cb/clean_tweets_2022_01_January.RData?dl=1"))
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