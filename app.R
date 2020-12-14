library(shiny)
library(shinyjs)
library(quantmod)
library(DT)



stockdata <- read.csv('GSPC.csv')
stockdata$Date <- as.Date(stockdata$Date,format = "%m/%d/%Y")

trade_data <<- NULL
useable_cash <<- 25000
close_value <- stockdata[stockdata$Date==as.Date("2020-12-01"),]$Close

shinyApp(
    ui = fluidPage(
        tabsetPanel(tabPanel("Advice",
                             fluidRow(column(12,h4("The investment advice on the market issued before July 1 can be found in these links.
                                        Hope you can make a better judgment on the stock trend using these available resources."),
                                             align="left"),
                                      column(12,a("https://www.fa-mag.com/financial-advisor-magazine/issues/2020/06", href="https://www.fa-mag.com/financial-advisor-magazine/issues/2020/06")),
                                      column(12,a("https://savantwealth.com/savant-views-news/market-commentary/market-update-june-2020/", href="https://savantwealth.com/savant-views-news/market-commentary/market-update-june-2020/")),
                                      column(12,a("https://www.ullandinvestment.com/2020/06/", href="https://www.ullandinvestment.com/2020/06/")),
                                      column(12,a("https://bluerocke.com/2020/06/", href="https://bluerocke.com/2020/06/")),
                                      column(12,a("https://atom.finance/", href="https://atom.finance/")),
                                      column(12,a("https://www.thinkadvisor.com/investment-advisor/issue/investment-advisor-june-2020-1/", href="https://www.thinkadvisor.com/investment-advisor/issue/investment-advisor-june-2020-1/")))),
                    tabPanel("Main",
                             
                             fluidRow(
                                 h3("Initial Position: 250000"),
                                 h4("An initial position of $250,000 in cash starting, you can spend as much as you want, just don’t exceed $250,000"),
                                 column(3,br(),
                                        selectInput(inputId = "select_date",
                                                    label = "Select the buy date",
                                                    choices = unique(stockdata$Date)
                                        ),
                                        numericInput("Buy","",value = 0),
                                        verbatimTextOutput("value"),
                                        actionButton("buy_action","Buy"),actionButton("clear_action","clear"),
                                        h4("You can choose any date from 07/01/2020 till 12/01/2020 to buy S&P500 stock. The final transaction in my project will return the portfolio to cash on 1 December 2020")
                                 ),
                                 column(9,
                                        h4("This is the stock trend from the day you start till 12/01/2020"),plotOutput("plot1",height = 600))),
                             br(),
                             fluidRow(h4("The table below is to track your portfolio using last close prices to track the value of individual positions, positions by category which is S&P 500, and portfolio value."),dataTableOutput("table1"))))
        
    ),
    
    server = function(input, output, session) {
        
        tabledata <- reactive({
            
            input$buy_action
            buy_open <- stockdata[stockdata$Date==isolate(input$select_date),]$Open
            if (useable_cash>=isolate(input$Buy)) {
                useable_cash <<- useable_cash - isolate(input$Buy)
                
                tempdata <- data.frame(Stock="S&P500",
                                       label1 = isolate(input$select_date),
                                       label2 = isolate(input$Buy),
                                       label3 = useable_cash,
                                       label4 = isolate(input$Buy)/buy_open*close_value-isolate(input$Buy)
                )
                names(tempdata) <- c("Stock","Trading Date","portfolio value","useable cash","return portfolio")
            }else{
                tempdata <- NULL
            }
            trade_data <<- rbind(trade_data,tempdata)
            trade_data
        })
        
        observeEvent(input$clear_action,
                     {trade_data <<- NULL
                     useable_cash <<- 25000}
        )
        
        numbers <- reactive({
            validate(
                need(is.numeric(input$Buy), "Please input a number")
            )
        })
        output$value <- renderPrint({ numbers() })
        
        
        observe({
            input$buy_action
            if(isolate(input$Buy) > isolate(useable_cash)){
                showModal(modalDialog(
                    title = "Somewhat important message",
                    "useable cash not enough",
                    easyClose = TRUE,
                    footer = NULL
                ))
            }
        })
        
        
        output$plot1 <- renderPlot({
            tempdata <- subset(stockdata,stockdata$Date>=input$select_date)
            S_P500  <- xts(tempdata[,-1], order.by=tempdata[,1])
            chartSeries(to.daily(S_P500), theme=chartTheme('white'))
        })
        
        output$table1 <- renderDataTable({
            tabledata()
        })
        
        
        
    }
)


