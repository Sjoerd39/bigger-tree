---
title: Speech generator
author: Sjoerd Dikkerboom
date: '2019-01-24'
slug: speech-generator
categories:
  - speech generator
  - AI
  - R
  - shiny
  - einsteinweek
tags:
  - speech generator
  - AI
  - R
  - shiny
  - einsteinweek
---

I was asked to introduce an Artificial Intelligence workshop for government employees (colleagues). I thought i could do that through an (AI) speech generator. So I made one. Incorporating some Language Processing techniques I recently worked with.

The generator contains the content of 45 speeches in Enlish, mainly of English and American statesman. The sentences are cleaned (minimally) and separated. Checked for their sentiment (positive or negative). In the app you can choose the overall sentiment of the speech and add your own welcoming and closing messages. Choose the number of lines. Press the button and voila! The App generates the speech for you (use at your own risk ;) ).

<iframe width="860" height="520" src="https://sjoerd.shinyapps.io/speechapp/"></iframe>

At the point of writing this post, i didn't do the introduction yet. I'm still debating if I'll use the speech generator for it =). 

The sentiment analysis was done through the `pattern.nlp` package, that uses python in the background. After I wrote this app I discovered `udpipe`. It's an easier package to use. I liked working with `pattern.nlp`, but I probably will use `udpipe` in the future.

The speeches were collected into an excel sheet. Email me if you would like the data. The preparation of the data happens offline before the app is deployed. So there are two parts to the code:



```r
library(tidyverse)
library(readxl)
library(tokenizers)
library(pattern.nlp)

df <- read_xlsx("speech.xlsx") 

list <- df$speech

## make seperate sentences from all the speeches (this is not perfect)
sentences <- lapply(list, tokenize_sentences)

## make a dataframe with the seperate sentences and a identifier (speaker + date)
## mainly because the above lapply makes a nested list (can be improved).
sp_list <- list()

for (i in 1:length(sentences)) {
  
  jj <- length(sentences[[i]][[1]])
  ll <- list()
  
  for (j in 1:jj) {
  ll[[j]] <- sentences[[i]][[1]][[j]]
  }
  
  sp_list[[i]] <- data.frame(sp_sc = matrix(unlist(ll)),
                             speaker_date = paste0(df$speaker[[i]], "_", df$date[[i]]))
}


df_speech <- bind_rows(sp_list)

## make a list of all the sentences, and loop it through the sentiment function
sentences <- df_speech$sp_sc
sent <- list()

for (i in 1:length(sentences)) {
  s <- pattern_sentiment(sentences[[i]], language = "english")
  sent[[i]] <- s
}

speech_pool <- bind_rows(sent) %>% 
  bind_cols(df_speech) %>% 
  ## do -some- cleaning
  filter(!str_detect(id, "thank|Thank")) %>% 
  filter(!str_detect(id, "president|President")) %>% 
  filter(nchar(id) > 25)

# write file for app
# write_rds(speech_pool, "speechaiapp/speech_pool.rds")
```

Code for the shiny app:


```r
library(shiny)
library(tidyverse)

speech_pool <- read_rds("speech_pool.rds") 
speech_pool <- speech_pool %>% 
  mutate(pool = "all") %>% 
  bind_rows(speech_pool %>% filter(polarity > 0.1) %>% mutate(pool = "pos")) %>% 
  bind_rows(speech_pool %>% filter(polarity < -0.1) %>% mutate(pool = "neg"))

# Define UI for application
ui <- fluidPage(
   # Application title
   titlePanel("Speech generator"),
   
   # Sidebar with a slider input for number sentences
   sidebarLayout(
      sidebarPanel(
         sliderInput("lines",
                     "Number of sentences:",
                     min = 2,
                     max = 20,
                     value = 5),
         # Radiobuttons for sentiment
         radioButtons("pos_neg", "Sentiment of the speech:",
                      choices = c("Positive" = "pos",
                                  "Negative" = "neg",
                                  "Doesn't matter" = "all"),
                      selected = "pos"),
         # welcome message and closing remarks
         textInput("welcome", "Welcoming message:", "Hello friends,"),
         textInput("close", "Closing remark:", "Thank you."),
         # button for generating speech
         actionButton("go", "Generate speech")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         htmlOutput("speech")
      )
   )
)

# Define server logic 
server <- function(input, output) {
  
  # generating the speech
  observeEvent(input$go, {
    
    output$speech <- renderUI({
      speech <- speech_pool %>%
        # filter speech pool based on selected sentiment
        filter(pool == input$pos_neg) %>% 
        select(id) %>%
        ## wrapped the input in isolate to stop the text from updating automaticly when input$lines changes
        sample_n(isolate(input$lines)) %>% 
        paste(collapse = " <p/> ") %>% 
        ## little bit of cleaning of the final 'draft'  of the speech
        str_replace_all("[^([:alnum:]|//.)]", " ") %>%
        str_sub(3, -3)
      
      welcome <- isolate(input$welcome)
      close <- isolate(input$close)
      
      HTML(paste(welcome,speech, close, sep="<p/>"))
      
      })
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
```


