## app.R ##

#runApp("C:/blockchain/ethereum/rparser/dashboard/")
library(shiny)
library(shinydashboard)
bigbang <- as.Date("2015-07-30")

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Ether mining profitability"
  ),
  dashboardSidebar(
    numericInput("hashrate", label = h4("Hashrate (MH/s)"), value = 250),
    numericInput("investment", label = h4("Investment ($)"), value = 7000),
    dateInput("investment_date", label = h4("Date of investment"), value = Sys.Date(), min = bigbang, max = Sys.Date() + 5),
    numericInput("ethsellprice", label = h4("Ether sell price ($)"), value = 300),
    actionButton("reset_input", "Reset inputs")
  ),
  dashboardBody(
    fluidRow(uiOutput("selectedText")),
    fluidRow(plotOutput("profitForecast")),
    fluidRow(
      column(6,plotOutput("nhrForecast")),
      column(6,plotOutput("detmForecast"))
    )
  )
)

#read the ethDaily data file
#dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
#ethDaily <- readRDS(paste0(dir,"/data/daily.Rda"))
library(forecast)
library(ggplot2)
library(scales)
library(dplyr)

ethDaily <- readRDS("data/daily.Rda")

server <- function(input, output,session) { 
  observeEvent(input$reset_input, {
    updateNumericInput(session, "hashrate", value = 250)
    updateNumericInput(session, "investment", value = 7000)
    updateDateInput(session,"investment_date" ,value  = Sys.Date())
    updateNumericInput(session, "ethsellprice", value = 300)
  })
  
  #userHashRate <- function(){return(250*1e6)}
  #investment <- function(){return(7000)}
  #investment_date <- function(){return(as.Date("2017-06-01"))}
  #ethPrice <-  function(){return(300)}
  
  #user defined inputs
  userHashRate <- reactive({ input$hashrate* 1e6 })  
  investment <- reactive({ input$investment })
  investment_date <- reactive({ input$investment_date }) 
  ethPrice <- reactive({ input$ethsellprice })
  
  #system inputs
  forecastPeriod <- 500  #days
  #remove last day from set since this day is yet incomplete
  ethDaily <- ethDaily[1:(nrow(ethDaily)-1),]
  #x labels for nhr and detm forcast
  num <- seq(1, nrow(ethDaily)+forecastPeriod,length.out=6)
  days <- seq(min(ethDaily$Day), max(ethDaily$Day)+forecastPeriod,length.out=6)
  #y labels for nhr
  number <- c(1e11,1e12,1e13,1e14,1e15)
  label <- c( "100 GH/s", "1 TH/s", "10 TH/s","100 TH/s","1 PH/s")  
  
  # NetHashRate forecaster
  nhr = auto.arima(ethDaily$netHash)
  f_nhr = forecast(nhr, h=forecastPeriod) 
  output$nhrForecast <- renderPlot({
    autoplot(f_nhr) + 
      scale_y_continuous(trans = log10_trans(),	breaks = number, 	labels = label, limits = c(1e11,1e15)) + 
      ylab("NetHash") + 
      scale_x_continuous(breaks = num, labels=days) +
      ggtitle("Total network prediction")+
      theme_bw()
    
  })
  
  # DailyEtherToMiner forecaster
  detm = auto.arima(ethDaily$ethTotal)
  f_detm = forecast(detm, h=forecastPeriod) 
  output$detmForecast <- renderPlot({
    autoplot(f_detm) + 
      ylab("Ether") + 
      scale_x_continuous(breaks = num, labels=days)+ 
      ggtitle("Total daily ether rewards for miners")+
      theme_bw()
  })
  
  output$profitForecast <- renderPlot({
    #create a generic df containing the nhr and detm actual and forecasted
    df <- data.frame(date = seq(bigbang,max(ethDaily$Day)+forecastPeriod, by=1),
                     nhr = c(f_nhr$x,f_nhr$mean),
                     detm = c(f_detm$x, f_detm$mean))
    
    #subset the df as of when the user did (plans) its investment
    df_user <- subset(df, date >= investment_date())
    
    #calculate user hashrate relative to nhr
    df_user$pofnhr <- userHashRate() / df_user$nhr
    #calculate cumulative user expected daily ether reward
    df_user$cser <- cumsum(df_user$pofnhr*df_user$detm)
    #calculate total value of users ether balance at the given ether price
    df_user$csdu <- df_user$cser * ethPrice()
    #breakeven, other dividends not taken into account
    df_user$be <- df_user$csdu - investment()
    
    #determine breakeven position in df
    n_be <- which.min(abs(df_user$be))
    
    #breakeven date and in how many days is it archieved
    be_date <- df_user[n_be,1]
    be_days <- as.numeric(difftime(be_date,investment_date(),"days"))
    
    if(n_be == nrow(df_user)){
      title <- "Breakeven point is not reached with current selection."
    } else {
      title <- paste0("Expected breakeven at: ",be_date ," (",be_days," days)")
    }
    
    ggplot(df_user, aes(x = date, y=csdu))+ 
      xlab("date") +
      ylab("Earnings ($)")+
      geom_line()+
      #investment line
      geom_hline(yintercept = investment(), color ="red", linetype = "dashed") + 
      geom_text(aes(x=min(df_user$date)+30, y= investment()+0.05*investment(), label = paste0(investment(),"$ investment"))) +
      #today date line
      geom_vline(xintercept = as.numeric(Sys.Date()), color = "grey", linetype = "dashed")+
      geom_text(aes(x=Sys.Date(), y= investment()/100, label = "Today")) +
      theme_bw() +
      ggtitle(title)  +
      scale_y_continuous(labels = comma)
    
  })
  
  
}

shinyApp(ui, server)