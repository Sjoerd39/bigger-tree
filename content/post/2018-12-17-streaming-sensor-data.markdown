---
title: streaming sensor data
author: Sjoerd Dikkerboom
date: '2018-12-17'
slug: streaming-sensor-data
categories:
  - R
  - streaming data
  - API
  - Smart emission
  - Nijmegen
  - Temperature
  - Sensor
  - forecast
tags:
  - R
  - Streaming data
  - forecast
  - APi
---

At the municipality i work at we increasingly want to anlyse and publish real time data. As probably every organization =). This is a small practice project where i want to gather live data. Analyze it. And present it in a live, self-updating dashboard.


The data i gather is from sensors placed in the city of Nijmegen for the <a href="http://smartemission.ruhosting.nl/visitors/" target="_blank">Smart Emissions project</a>. Citizens of Nijmegen and the municipality placed several cheap sensors around town to measure temperatures, content of the air (PM 2,5, NO2 etc.) and sometimes even sound. They provide several API's to access the data.

The dashboard I made is presenting the measurements of sensor '20080007' of the last 12 hours. Plus it presents a forecast of the next 6 hours based on the last 1000 measurements. It tries to update itself every 5 minutes. It might take a minute to load.



<iframe width="860" height="400" src="https://sjoerd.shinyapps.io/temp_app/"></iframe>


Below is the script for making this simple dashboard.



```r
library(shiny)
library(jsonlite)
library(tidyverse)
library(shiny)
library(forecast)
library(tseries)
library(janitor)


url <- "https://data.smartemission.nl/gost/v1.0/Things?$filter=name%20eq%20%2720080007%27&$expand=Datastreams/Observations($top=1000)"


ui <- shinyServer(fluidPage(
  plotOutput("plot_temp")
))


server <- shinyServer(function(input, output, session){
  # Function to get new observations
  get_temp <- function(){
    ## update every 2 mins
    tmp <- fromJSON(url, flatten = TRUE)
    
    df <- tmp$value$Datastreams[[1]]$Observations[[5]] %>% 
      clean_names() %>% 
      mutate(date_result = as.Date(substr(max(as.Date(result_time)), 1, 10)),
             time = substr(result_time, 12, 19))
    
    Sys.sleep(0.1) #ensures API results are returned successfully
    
    df
  }
  
  output$plot_temp <- renderPlot({
    
    ## make this function to update itself every 300000 ms, or 5 mins
    invalidateLater(300000, session)
    
    # Initialize df
    df <- get_temp()
    
    ## get times for forecast
    last_obs <- df %>% 
      select(time) %>% 
      head(1)
    
    times <- seq(as.POSIXct(last_obs$time, format = "%H:%M:%S") + 3600, length.out = 6,  by = "hour")
    
    ## making a timeseries for forecasting
    ts <- df %>% 
      arrange(result_time) %>% 
      select(result) %>% 
      ts(frequency = 24)
    
    ## forecasting with auto.arima
    fit <- auto.arima(ts)
    
    fc <- as.data.frame(forecast(fit, h = 6)) %>% 
      ## add times for plotting later
      bind_cols(time = times) %>% 
      mutate(time = strftime(time, format = "%H:%M:%S")) %>% 
      rename(pt_fc = `Point Forecast`) %>% 
      select(time, pt_fc)
    
    ## make the 24h of a day for plotting later
    h24 <- strftime(seq(as.POSIXct("00:00:00", format = "%H:%M:%S") , length.out = 24,  by = "hour"), format = "%H:%M:%S")
    
    date_title <- df[[1,14]]
    
    # Plot the 12 most recent values
    df %>% 
      head(12) %>%
      mutate(type = "Temperature measurements") %>% 
      ## add forecast points to data
      bind_rows(fc) %>%
      ## add 24h times for better plotting
      full_join(data.frame(time = h24)) %>%
      ## format time to better fit the plot
      mutate(time = format(as.POSIXct(time, format = "%H:%M:%S"), "%H:%M")) %>% 
      ## start the plot
      ggplot() +
      # add measurements
      geom_line(aes(x = time, y = result, group =  1, linetype = "Temperature measurements"), colour = "orangered1", size = 2) +
      # add forecast
      geom_point(aes(x = time, y = pt_fc, shape = "Forecast"), colour = "steelblue1", size = 6) +  
      xlab(" ") +
      ylab("Temperature") +
      ggtitle(label = paste0("Temperature for station with id '20080007' on ", date_title, ". Will try to update every 5 minutes.")) +
      theme(legend.title=element_blank())

    
   
  })
})

shinyApp(ui=ui,server=server)
```


I'm sure there is some tweaking possible to make it faster or better. For me it was a good practice to work with continues, streaming data. And to work with Shiny again. It also opens a lot possibilities for fun future projects =).

One thing I'll definitely look at is to also make models independent of long time series. And let the last few measurements update the model in small increments. It will be a lot faster. Have fun with your own streaming data and let me know of your own ideas!

