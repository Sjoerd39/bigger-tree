---
title: 2 animated race shiny app
author: Sjoerd Dikkerboom
date: '2019-10-12'
slug: 2-animated-race-shiny-app
categories:
  - 7hills
  - shiny
  - R
  - running
tags:
  - 7hills
  - shiny
  - gganimate
---

Post number 2? Yes! In an earlier [post](https://www.bigger-tree.org/2018/11/07/running-a-race-animated/) I made an animation of a running race. Since then I wanted to make a shiny app out of this idea so that people can add themselves on the track and animate their own race. And now it's finally finished! You can try it for yourself here: [seven hills race app](https://sjoerd.shinyapps.io/run_anim_app/). 

I'll explain some of the more important components of the app. The first thing is processing the user input. I experimented with a lot of different methods, including the `R timeInput` from the [shinyTime package](https://github.com/burgerga/shinyTime), but the easiest way i found is a combination of free text input and some text processing. Only downside is that input without the required structure will not work.


```r
textAreaInput(label = "", inputId = "kmtimes", width = "250px", height = "320px",
                  placeholder =  "00:02:45
                  00:05:28
                  00:08:19
                  00:11:08
                  00:14:07
                  00:16:53
                  00:19:37
                  00:22:30
                  00:25:17
                  00:27:49
                  00:30:38
                  00:33:19
                  00:35:54
                  00:38:28
                  00:41:05"),
```

The text processing consists of splitting the output in different lines and detecting the time input from the user. And then add some missing information for later processing and important for the labelling. 


```r
  ## input times processing
  user_data <- eventReactive(input$run, {
    text <- input$kmtimes
    text <- strsplit(text, "\n") %>% 
      as.data.frame()
    
    names(text) <- c("time")
    
    text <- text %>% 
      mutate(time = as.character(time),
             time = str_extract(time, "[0-9]*:*[0-9]+:[0-9]+"),
             time = str_pad(time, 8, side = "left", pad = "0"),
             time = ifelse(str_detect(time, "000") == T, str_replace(time, "000", "00:" ), time),
             runners = "you",
             km = row_number())
    })
```

To generate the gif in the output a regular `R imageOutput` can be used. On the server side, you have to make sure that there is a temporary outfile created. And make sure the `R anim_save` command writes the animation to this outfile. Plus make sure that the eventual file is deleted after it's made to make space for a new animation.



```r
## in UI
imageOutput("plot1")

## in server

      outfile <- tempfile(fileext='.gif')

      p <- ggmap(m) +
        scale_x_continuous(limits = c(min(route_times$X), max(route_times$X)), expand = c(0.0015, 0.0015)) +
        scale_y_continuous(limits = c(min(route_times$Y), max(route_times$Y)), expand = c(0.0015, 0.0015)) +
        geom_point(data = route_times_15sec, aes(x = X, y = Y,
                                                 color = as.factor(runners)), size = 5)+
        geom_label_repel(data = route_times_15sec,
                         aes(x = X, y = Y,
                             fill = as.factor(runners),
                             label = label_t),
                         size = 4, fontface = "bold",
                         box.padding = 0.35, point.padding = 1,
                         nudge_y = 0.0025, nudge_x = 0.02) +
        geom_label(data = race_time,
                   aes(x =  5.867, y = 51.79665,label = label_time),
                   size = 4.5, color = "black", fill = " white", fontface = "bold", hjust = "left") +
        theme_void() +
        theme(legend.position = "none") +
        # gg animate options
        transition_reveal(sec15) +
        ease_aes("linear")
      
      anim_save("outfile.gif", animate(p, nframes = nr_frames(), width = 400, height = 400))
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif')
           
        
      })
      }, deleteFile = TRUE)
```

The full code for the app is available at github: https://github.com/Sjoerd39/7hills_app. In a few weeks a new edition of the seven hills running race will be held. So then I'll update this app to contain the new times.

I'm very pleased this app works now! In future post I'll talk about how to deploy this app to an AWS EC2 instance and how to give it it's own domain so you can acces it from a regular webadress.
