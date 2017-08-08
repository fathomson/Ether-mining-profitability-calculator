# Frank Thomson
# f.a.thomson@gmail.com

# devtools::install_github(c("ramnathv/htmlwidgets", "rstudio/dygraphs"))

# Load libraries
library(shiny) 
library(shinyjs)
library(shinydashboard)
library(forecast)
library(dplyr)
library(dygraphs)
library(jsonlite)
library(xts)

# the start of it all.
bigbang <- as.Date("2015-07-30")

# dev mode - RStudio
# dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
# ethDaily <- readRDS(paste0(dir,"/data/daily.Rda"))
ethDaily <- readRDS("data/daily.Rda")
ethDaily$day <- as.Date(ethDaily$day)

# shiny ui
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Ether mining profitability calculator ",
    titleWidth = 500
  ),
  dashboardSidebar(
    useShinyjs(),
    sidebarMenu(
      numericInput("hashrate", label = h4("Hashrate (MH/s)"), value = 250),
      numericInput("investment", label = h4("Investment ($)"), value = 7000),
      checkboxInput("advanced_options", "Show advanced options"), 
      numericInput("forecast_days", label = h4("Days to forecast"), value = 500, min = 10, max = 1000),
      dateInput("investment_date", label = h4("Date of investment"), value = Sys.Date(), min = bigbang, max = Sys.Date() + 5),
      dateInput("predict_from",label = h4("Predict from"), value = max(ethDaily$day)-1, min = max(ethDaily$day) - 50, max = max(ethDaily$day)-1),
      numericInput("ethsellprice", label = h4("Ether sell price ($)"), value = 300),
      numericInput("energy_usage", label = h4("Energy consumption (kWh)"), value = 1),
      numericInput("energy_price", label = h4("Energy price ($/kWh)"), value = .10, step = .1, min = 0),
      actionButton("reset_input", "Reset inputs"),
      menuItem("Source code", icon = icon("github"), 
               href = "https://github.com/fathomson/eth_cloudminingprofitability"),
      menuItem("Frank Thomson", icon = icon("linkedin"), 
               href = "https://www.linkedin.com/in/fathomson/")
    )
  ),
  dashboardBody(
    fluidRow(
      box(
        title = "Instructions ", width = 12, status = "primary", solidHeader = TRUE,
        collapsible = TRUE,
        h4("Usage"),
        helpText("In the left sidebar you can enter your hashrate and investment cost. Click on 'Show advanced options' to change the days to forecast, date of investment, ether sell price, energy consumption and energy price.
                 If you are a cloud miner you can set the energy price to 0. Based on the values you provided this app calculates the expected ROI and ether mined after the forecasted period. You can use the chart to see your returns
                 at an earlier date. You can zoom in by selecting a specific period in the chart, double clicking resets your zoom."),
        h4("Tabs"),
        helpText("Below you can see three tabs: ROI, Net hash rate forecast and Daily ether to miners. The ROI (return on invesment) tab shows the rate at which you are earning money. The net hash rate forecast tab show you a forecast 
                 of the predicted future hashrate. The daily ether to miners tab show you the predicted amount of ether which is rewarded to the miners. The last two tabs contain details on how the forecasts are created, the code of this
                 application is publicly available on github (see link in the menu)")
      )
    ),
    fluidRow(
      tabBox(title = "", width=12,
             tabPanel(title=tagList(shiny::icon("line-chart"), "ROI"),
                      fluidRow(
                        infoBoxOutput("be",width=3),
                          infoBoxOutput("roi",width=3),
                          infoBoxOutput("tem",width=3),
                          infoBoxOutput("rep",width=3)
                        
                      ),
                      fluidRow(
                        column(10, offset = 1,
                          dygraphOutput("expectedEarnings", height = 600)
                        )
                      )
             ),
             tabPanel(title=tagList(shiny::icon("shower"), "Net hash rate forecast"),
                      fluidRow(
                        column(width = 12, 
                          helpText("The daily net hash rate is calculated by dividing the weighted difficulty by the mean block time (see github). The forecast is done with the auto.arima model in the forecast package of Rob Hyndman, 
                               on the bottom right of the chart you can see the model parameters. The net hash rate is required to determine the likelyhood of you, with your hashrate, finding a block. Do note the the y-axis is log scaled.")
                          )
                      ),
                      fluidRow(
                        column(10, offset = 1,
                               dygraphOutput("nhrForecast"), height = 600
                        )
                      )
                      
             ),
             tabPanel(title=tagList(shiny::icon("money"), "Daily ether to miners"),
                      fluidRow(
                        column(width = 12, 
                               helpText("The daily ether to miners show the amount of ether which is rewarded daily to the miners. This includes the 5 eth block reward, the uncle finders and includers fee and rewards for included transactions (see github).
                                        Together with your percentage of the net hash rate we can predict how much ether you are likely to earn in the future.")
                               
                        ),
                      fluidRow(
                        column(10, offset = 1,
                               dygraphOutput("detmForecast"), height = 600
                        )
                      )
                      )
                      )
))))

# shiny server
server <- function(input, output,session) { 

  # The last row is not a full day yet, exclude from forecasts.
  ethDaily <- ethDaily[1:(nrow(ethDaily)-1),]
  
  # reset inputs to defaults when reset button is clicked.
  observeEvent(input$reset_input, {
    updateNumericInput(session, "forecast_days", value = 500)
    updateNumericInput(session, "hashrate", value = 250)
    updateNumericInput(session, "investment", value = 7000)
    updateDateInput(session,"investment_date" ,value  = Sys.Date())
    updateDateInput(session, "predict_from", value = max(ethDaily$day))
    updateNumericInput(session, "ethsellprice", value = 300)
    updateNumericInput(session, "energy_usage", value = 1)
    updateNumericInput(session, "energy_price", value = 0.1)
  })
  
  # show or hide advanced options, 
  advanced_options <- reactiveValues(yes = TRUE)
  observeEvent(input$advanced_options, {
    if(advanced_options$yes){
      shinyjs::hide("forecast_days")
      shinyjs::hide("investment_date")
      shinyjs::hide("predict_from")
      shinyjs::hide("ethsellprice")
      shinyjs::hide("energy_usage")
      shinyjs::hide("energy_price")
      shinyjs::hide("reset_input")
    } else {
      shinyjs::show("forecast_days")
      shinyjs::show("investment_date")
      shinyjs::show("predict_from")
      shinyjs::show("ethsellprice")
      shinyjs::show("energy_usage")
      shinyjs::show("energy_price")
      shinyjs::show("reset_input")
    }
    advanced_options$yes <- !advanced_options$yes
  })  
  
  # dev mode
  # advanced_options <- data.frame(yes=TRUE)
  # userHashRate <- function(){return(250*1e6)}
  # investment <- function(){return(700)}
  # investment_date <- function(){return(Sys.Date())}
  # predict_from <- function(){return(as.Date("2017-08-03"))}
  # ethPrice <-  function(){return(300)}
  # forecast_days <-  function(){return(500)}
  # energy_usage <-  function(){return(1)}
  # energy_price <-  function(){return(0)}

  # user input values
  forecast_days <- reactive({ input$forecast_days })  
  userHashRate <- reactive({ input$hashrate* 1e6 })  
  investment <- reactive({ input$investment })
  investment_date <- reactive({ input$investment_date }) 
  predict_from <- reactive({ input$predict_from }) 
  ethPrice <- reactive({ input$ethsellprice })
  energy_usage <- reactive({ input$energy_usage })
  energy_price <-  reactive({ input$energy_price })
  
  # functions.r ?
  #
  # dygraph needs to have the data in sepecif xts format, convert forecast model to this format.  
  convertToDygraph <- function(forecast){
    past_dates <- seq(bigbang,as.Date(last(ethDaily$day)),by="days")
    predictions_from <- min(predict_from(),as.Date(last(ethDaily$day)))
    future_dates <- seq(predictions_from,as.Date(last(ethDaily$day))+forecast_days()-1,by="days")
    a <- xts(forecast$x_full, order.by = past_dates)
    colnames(a) <- "actuals"
    b <- xts(forecast$lower[,2], order.by = future_dates)
    colnames(b) <- "lower"
    c <- xts(forecast$mean,  order.by = future_dates)
    colnames(c) <- "mean"
    d <- xts(forecast$upper[,2], order.by = future_dates)
    colnames(d) <- "upper"
    return(cbind(a,b,c,d))
  }
  
  #  calculate data based on user inputs.
  calculateData <- reactive({
    # forecast net hash rate
    ethDailyPAST <- subset(ethDaily, day <= predict_from())
    nhr = auto.arima(ethDailyPAST$netHash)
    forecastpastdays = round(as.numeric(difftime(last(ethDaily$day),predict_from(), unit="days")),0)
    f_nhr = forecast(nhr, h=forecastpastdays+forecast_days()) 
    # predict the past
    f_nhr$x_full <- ethDaily$netHash
    
    # forecast daily ether to miners
    detm = auto.arima(ethDailyPAST$ethTotal)
    f_detm = forecast(detm, h=forecastpastdays+forecast_days()) 
    f_detm$x_full <- ethDaily$ethTotal
    
    # create a dataframe with the forecasts
    df <- data.frame(date = seq(bigbang,as.Date(last(ethDaily$day))+forecastpastdays+forecast_days(), by=1),
                     nhr = c(f_nhr$x_full,f_nhr$mean),
                     detm = c(f_detm$x_full, f_detm$mean))
    
    # subset the df based on investment date of user
    df_user <- subset(df, date >= investment_date())
    
    # percentage of net hash rate that user mines at
    df_user$pofnhr <- userHashRate() / df_user$nhr
    
    # cumulative daily ether reward to user
    df_user$cser <- cumsum(df_user$pofnhr * df_user$detm)
    
    # enery costs based on hours a day (24 for simplicity), energy price, energy usage and if advanced options is ticked.
    df_user$ec <- seq.int(nrow(df_user)) * (24*energy_price()*energy_usage()*advanced_options$yes)

    # cumulative ether value of user at a set price
    df_user$csdu <- df_user$cser * ethPrice()
    
    # net profit
    df_user$csdue <-  df_user$csdu - df_user$ec
    
    # total earnings on a specific date
    df_user$be <- df_user$csdu - investment() - df_user$ec
    
    # calculate breakeven point. can be parabolic due to energy costs
    # get first positive value
    pos_first_be <- which(df_user$be>0)[1] 
    
    # when never getting positive set to end of forecast period
    if(is.na(pos_first_be)) 
      pos_first_be <- forecast_days()
    
    # used to fetch be date
    n_be <- ifelse(pos_first_be < forecast_days(),pos_first_be, forecast_days())
    
    # what is the lucky date when I have earned my investment back?
    be_date <- df_user[n_be,1]
    be_days <- as.numeric(difftime(be_date,investment_date(),"days"))
    
    # user dataframe, breakeven (bool), breakevendate and days.
    # cleanup mess one day.
    list(f_nhr = f_nhr,
         f_detm = f_detm,
         df_user = df_user,
         tem = round(max(df_user$cser),2),
         rep = round(investment()/max(df_user$cser)),
         n_be = (n_be == forecast_days()),
         be_date = be_date,
         be_days = be_days,
         eb_p  = round((last(df_user$csdue)/investment())*100),1)
  })
  
  #
  # Inforboxes 
  #
  
  # render info box break even
  
  # render info box tem
  output$be <- renderInfoBox({
    if(calculateData()$n_be){
      infoBox(
        paste0("Break even at:"), "Not within forecast", icon = icon("calendar"),
        color = "red", fill = TRUE
)
    } else {
      infoBox(
        paste0("Break even at:"), calculateData()$be_date, icon = icon("calendar"),
        color = "green", fill = TRUE
      )
    }
  })
  
  
  # render info box ROI,
  output$roi <- renderInfoBox({
    if(calculateData()$n_be){
      infoBox(
        paste0("ROI after ", forecast_days() , " days:"), paste0(calculateData()$eb_p, "%"), icon = icon("thumbs-down"),
        color = "red", fill = TRUE
      )
    } else {
      infoBox(
        paste0("ROI after ", forecast_days() , " days:"), paste0(calculateData()$eb_p, "%"), icon = icon("thumbs-up"),
        color = "green", fill = TRUE
      )
    }
  })
  
  # render info box tem
  output$tem <- renderInfoBox({
    infoBox(
      paste0("Ether mined after ", forecast_days() , " days:"), calculateData()$tem, icon = icon("diamond"),
      color = "blue", fill = TRUE
    )
  })
  
  # render info box rep
  output$rep <- renderInfoBox({
    infoBox(
      "Required ether price to break even", paste0("$",calculateData()$rep), icon = icon("money"),
      color = "green", fill = TRUE
    )
  })
  
  # expected earnings chart
  output$expectedEarnings <- renderDygraph({
    
    # format data in such a way that dygraph can handle it.
    df_user <- calculateData()$df_user
    
    # create timeseries with energy
    if(sum(df_user$ec)>0){
      earnings_ts = xts(df_user$csdu,  order.by=df_user$date)
      colnames(earnings_ts) <- "Gross.profit"
      energy_ts = xts(df_user$ec,  order.by=df_user$date)
      colnames(energy_ts) <- "Energy"
      profit_ts = xts(df_user$csdue,  order.by=df_user$date)
      colnames(profit_ts) <- "Net.profit"
      ts <- cbind(earnings_ts,energy_ts,profit_ts)
    } else {
      profit_ts = xts(df_user$csdu,  order.by=df_user$date)
      colnames(profit_ts) <- "Net.profit"
      ts <- profit_ts
    }
    
    dg <- dygraph(ts, main = "At what rate do I earn my money back?") %>% 
     dyLegend(show = "auto") %>%
      dyLimit(investment(), color = "red", label = "Investment", labelLoc = c("right")) %>%
      dyEvent(Sys.Date(), "Today", labelLoc = "top") %>%
      dyAxis("x", label = "Date")  %>%
      dyAxis("y", label = "Earning ($)", valueRange = c(0, max(1.1*investment(),max(1.1*df_user$csdu))))  
    
    # add energy costs when applicable
    if(sum(df_user$ec)>0){
      dg %>%  
        dySeries("Gross.profit", label = "Gross.profit ($)", strokeWidth = 1, color = "blue") %>%
        dySeries("Energy", label = "Energy ($)", strokeWidth = 1, color = "black")  %>%
        dySeries("Net.profit", label = "Net profit ($)", strokeWidth = 3, color = "green") 
    } else {
      dg %>%
        dySeries("Net.profit", label = "Net profit ($)", strokeWidth = 3, color = "green") 
    }


  })
  

  # net hash rate forecast chart
  output$nhrForecast <- renderDygraph({
    # hash rate formatting
    # source https://stackoverflow.com/questions/15900485/correct-way-to-convert-size-in-bytes-to-kb-mb-gb-in-javascript
    hr_format <- 'function formatBytes(a,b){if(0==a)return"0 Bytes";var c=1e3,d=b||2,e=["Hashes/s","KH/s","MH/s","GH/s","TH/s","PH/s","EH/s","ZH/s","YH/s"],f=Math.floor(Math.log(a)/Math.log(c));return parseFloat((a/Math.pow(c,f)).toFixed(d))+" "+e[f]}'
    
    # get forecasted values
    f_nhr <- calculateData()$f_nhr
    # convert forecasted values such that dygrapgh can handle them
    cdg <- convertToDygraph(f_nhr)
    
    #annotation position at roughly 4/5 of the graph
    forecast_days_seq <- seq(start(cdg),end(cdg),by="days")
    annotation_position <- forecast_days_seq[floor(length(forecast_days_seq)*.8)]
    
    dygraph(cdg, main = "Actual and predicted net hash rate") %>%
      dySeries(name = "actuals", label = "Actual") %>%
      dySeries(c("lower","mean","upper"), label = "Predicted") %>%
      dyAxis("y", logscale = TRUE,  valueFormatter = hr_format, axisLabelFormatter = hr_format ) %>%
      dyLegend(show = "auto") %>%
      dyAxis("x", label = "Date")  %>%
      dyAxis("y", label = "Net hash rate")   %>%
      dyAnnotation(annotation_position, f_nhr$method, attachAtBottom = TRUE, width = 100)
  })
  
  # daily ether to miner forcast chart
  output$detmForecast <- renderDygraph({
    
    # plot can take some time, show progress to user
    withProgress(message = 'Making plot', {
      incProgress(1/2)
      # get forecast values
      f_detm <- calculateData()$f_detm
      incProgress(1/4)
      # convert forecasted values such that dygraph can handle them
      cdg <- convertToDygraph(f_detm)
      incProgress(1/5)
    })
    
    #annotation position at roughly 4/5 of the graph
    forecast_days_seq <- seq(start(cdg),end(cdg),by="days")
    annotation_position <- forecast_days_seq[floor(length(forecast_days_seq)*.8)]
    
    dygraph(cdg) %>%
      dySeries(name = "actuals", label = "Actual") %>%
      dySeries(c("lower","mean","upper"), label = "Predicted")%>%
      dyLegend(show = "auto") %>%
      dyAxis("x", label = "Date")  %>%
      dyAxis("y", label = "Daily ether to miners")  %>%
      dyAnnotation(annotation_position, f_detm$method, attachAtBottom = TRUE, width = 100)
  })
}

shinyApp(ui, server)