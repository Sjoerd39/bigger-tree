---
title: Generative animated art with spotifyR
author: Sjoerd Dikkerboom
date: '2019-06-05'
slug: generative-animated-art-with-spotifyr
categories:
  - generative art
  - R
  - gganimate
  - spotify
  - spotifyR
tags:
  - gganimate
  - generative art
  - R
  - spotifyR
  - spotify
---

So I came across the awesome Spotifyr [spotifyr](https://github.com/charlie86/spotifyr) package earlier this year. A package that lets you easily explore the extensive Spotify API within R. And when I read [this article](https://quantixed.org/2019/05/26/turn-a-square-generative-art/) about generative art on Quantixed made in R it got me thinking about combining these two.

The end result is a continuously updating 'art piece' made in Shiny. Generated by the computer with input from the music I play on Spotify. Super Awesome! 

*(might take a minute to load)*
<iframe width="600" height="500" src="https://sjoerd.shinyapps.io/apprt/"></iframe>

There is something happening here, but what's going on? After the first time the app is loaded it checks every 90 seconds if I played a new song. If so it will make a new animation between two songs. Otherwise it will display the earlier one. 

Different properties of the plots are controlled by the properties of the song retrieved from the Spotify api:

* The color depends on the valence of the song, how positive the song is.
* The repetition of the colors is controlled by the energy of the song.
* The distortion (hysteresis) of the squares is controlled by the ‘danceability’ of the song.

The code is updated from the [Quantixed](https://quantixed.org/2019/05/26/turn-a-square-generative-art/) to use tidyverse code and packages mainly to use `gganimate` to animate the plot.

The main function to generate the squares consists of a nested loop creating the coordinates square by square. It still uses the method of having a function control the distortion of squares based on a half sine wave. So squares on the edges have less deformation than squares in the middle. The height and width of the grid and the space between the squares (grout) are constant in this adaptation.


```r
make_grid_art <- function(grout, hFactor) {
  xWave <- seq.int(1:15)
  yWave <- seq.int(1:10)
  nSquares <- length(xWave) * length(yWave)
  x <- 0
  halfGrout <- (1 - grout) / 2
  for (i in seq_along(yWave)) {
    yCentre <- yWave[i]
    for (j in seq_along(xWave)) {
      if(hFactor < 1) {
        hyst <- rnorm(8, halfGrout, 0)
      }
      else {
        hyst <- rnorm(8, halfGrout, sin(x / (nSquares - 1) * pi) / hFactor)
      }
      xCentre <- xWave[j]
      x1 <- xCentre + hyst[2]
      x2 <- xCentre + hyst[4]
      x3 <- xCentre - hyst[6]
      x4 <- xCentre - hyst[8]
      y1 <- yCentre + hyst[1]
      y2 <- yCentre - hyst[3]
      y3 <- yCentre - hyst[5]
      y4 <- yCentre + hyst[7]
      new_shape <- data.frame(x = c(x1, x2, x3, x4), 
                              y = c(y1, y2, y3, y4)) %>% 
        mutate(id = (i * 10000) + j)
      # new_shape_end <- rbind(rt,rb,lb,lt)
      # new_shape <- cbind(new_shape_start,new_shape_end)
      if(i == 1 && j == 1) {
        df <- new_shape
      } else {
        df <- rbind(df,new_shape)
      }
      x <- x + 1
    }
    
  }
  df
}
```

This function makes the coordinates for a grid of (kind of) squares of 15 by 10, that can be plotted by `ggplot` and `geom_polygon`. 

In the setup there are a few environment variables defined to access the Spotify api. See https://developer.spotify.com/documentation/web-api/quick-start/ to setup an account for yourself. Then the first check is done and the most recently track id is stored as an environment variable for further use. Also the different color scales are defined,



```r
library(shiny)
library(tidyverse)
library(gganimate)
library(spotifyr)
library(wesanderson)
library(transformr)
source("fun_make_squares.R")

Sys.setenv(SPOTIFY_CLIENT_ID = <spotify_id>)
Sys.setenv(SPOTIFY_CLIENT_SECRET = <spotify_secret>)
atoken <- get_spotify_access_token()
recent_tracks <- get_my_recently_played(1)
track_id_old <- recent_tracks$track.id[[1]]

## use valance to choose color scale
clrscales <- data.frame(name = as.character(c("Cavalcanti1", "BottleRocket1", "GrandBudapest1",
                                              "Rushmore1", "Moonrise3", "GrandBudapest2", 
                                              "Zissou1", "Darjeeling1", "FantasticFox1")),
                        scale_id = rep(1:3),
                        mood = rep(1:3, each = 3),
                        stringsAsFactors = FALSE)
```

The shiny UI just calls for the animation to be shown. The server function exists of two parts. A small check, every 90 seconds to see if the most recently track has changed. If so then update the earlier defined track_id_old variable.

The main part will only update when track_id_old changes and begins with calling the Spotify api to get the two latest track id's. Get the track information and audio features. The set of audio features includes things like the danceability, energy and valance of a track. These features are picked to feed into the hysteresis part of the function call. The color scale is picked according to the valance. And energy is used to calculate the repetition of the colors. After repeating this for the second time, both data frames with the information about the grid of squares are fed into `ggplot` and animated with `gganimate`. Saving it within a list makes it that the animation is stored and displayed within the app. Voila!


```r
ui <- fluidPage(
  mainPanel(
    imageOutput("plot1")
    
  )
)

server <- function(input, output) {
  
  observe({
    invalidateLater(90000)
    recent_tracks <- get_my_recently_played(1)
    track_id_current <- recent_tracks$track.id[[1]]
    if(track_id_current != track_id_old){
      track_id_old <- track_id_current
    } 
  })
  
  observe({
    
      output$plot1 <- renderImage({
      
      # A temp file to save the output.
      outfile <- tempfile(fileext='.gif')
      
      track_id_old <- track_id_old
      
      recent_tracks <- get_my_recently_played(2)
      
      track_id1 <- recent_tracks$track.id[[2]]
      track_id2 <- recent_tracks$track.id[[1]]
      
      taf1 <- get_track_audio_features(track_id1)
      taf2 <- get_track_audio_features(track_id2)
      
      tmeta1 <- get_track(recent_tracks$track.id[[2]])
      tmeta2 <- get_track(recent_tracks$track.id[[1]])
      
      title1 <- paste0('"', tmeta1$name[1], '" by ', tmeta1$artists$name[1])
      title2 <- paste0('"', tmeta2$name[1], '" by ', tmeta2$artists$name[1])
      
      #### track 1 ####
      ## map the inverse of danceabilty (0 - 1), to hysteris (1-30)
      hst1 <- abs(taf1$danceability - 1) * 30
      
      clrscl1 <- clrscales %>% 
        filter(mood == ceiling(taf1$valence * 3)) %>% 
        filter(scale_id == sample(1:3, 1)) %>% 
        select(name)
      
      ## plot colors according to the energy 
      ## energy has better distribution than for example loudness
      ## divide it by a small nr (1.2) to allow for some repition of colors in 
      ## high energy songs
      nrgy1 <- sin(seq(1,15, (abs(taf1$energy)/1.2)))[1:15]
      nrgy1 <- data.frame(energy = c(nrgy1)) %>% 
        mutate(column_id = 1:15)
      
      df_at1 <- make_grid_art(0.2, hst1)
      
      df_at1 <- df_at1 %>% 
        mutate(column_id = rep(rep(1:15, each = 4), 10),
               state = title1) %>% 
        left_join(nrgy1, by = c("column_id"))
      
      #### track 2 ####
      ## map the inverse of danceabilty (0 - 1), to hysteris (1-30)
      hst2 <- abs(taf2$danceability - 1) * 30
      
      clrscl2 <- clrscales %>% 
        filter(mood == ceiling(taf2$valence * 3)) %>% 
        filter(scale_id == sample(1:3, 1)) %>% 
        select(name)
      
      nrgy2 <- sin(seq(1,15, (abs(taf2$energy)/1.2)))[1:15]
      nrgy2 <- data.frame(energy = c(nrgy2)) %>% 
        mutate(column_id = 1:15)
      
      
      df_at2 <- make_grid_art(0.2, hst2)
      
      df_at2 <- df_at2 %>% 
        mutate(column_id = rep(rep(1:15, each = 4), 10),
               state = title2) %>% 
        left_join(nrgy2, by = c("column_id"))
      
      
      #### add it all together ####
      df_recent_tracks <- bind_rows(df_at1, df_at2)
      
      ## plot an animate it
      pl <- df_recent_tracks %>% 
        ggplot() +
        geom_polygon(aes(x = x, y = y, group = id, fill = energy)) +
        theme_void() +
        theme(legend.position = "none") +
        scale_fill_gradientn(colors = wes_palette(clrscl2$name, 100, type = "continuous")) +
        transition_states(state,
                          transition_length = 5,
                          state_length = 10) +
        ease_aes("quintic-in-out") +
        labs(caption = "{closest_state}") +
        theme(plot.caption = element_text(color = "grey30", size = 14, face = "bold", hjust = 0.5))
      
      anim_save("outfile.gif", animate(pl))
      
      # Return a list containing the filename
      list(src = "outfile.gif",
           contentType = 'image/gif')
      }, deleteFile = FALSE)
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
```

This project is really fun to do and there are a thousand ways to expand on this. And I probably will! Generative art is really interesting! And initiatives like ['Perception Engines'](https://medium.com/artists-and-machine-intelligence/perception-engines-8a46bc598d57) from Tom White are things I will keep following.
