---
title: Happy holidays!
author: Sjoerd Dikkerboom
date: '2019-12-25'
slug: happy-holidays
categories:
  - happy holidays
  - christmas tree
  - christmas
  - gganimate
  - R
tags:
  - R
  - gganimate
  - happy holidays
  - happy new year
  - BiggeR TRee
---

Happy holidays to you all! For a short project I turned the [population pyramid of the city Nijmegen, the Netherlands](https://public.tableau.com/shared/4RHQ9W23W?:display_count=y&:origin=viz_share_link) into a shiny christmas tree.

![](/post/2019-12-25-happy-holidays/happy_holidays.gif)

The rough translation of the text is 'have a good/ nice holiday and a significant happy new year'. The code for this tree you can see below. It has several sections, but the tree and the baubles are made in seperate dataframes but with the same time reference. In the end both layers are combined in one animation.


```r
#### libraries ####
library(tidyverse)
library(gganimate)

#### colours ####
col_tree <- c("forestgreen", "green4", "darkgreen")
col_balls <- c("magenta", "deeppink", "yellow1", "orange", "cyan", "deepskyblue1", "springgreen1", "green", 
               "gold", "chartreuse1", "aquamarine1", "darkorchid", "plum1", "tomato", "red2", "peachpuff1")

#### data tree #####
data <- structure(list(age_grp = c("0-4", "0-4", 
                                           "05-09", "05-09", 
                                           "10-14", "10-14", 
                                           "15-19", "15-19", 
                                           "20-24", "20-24", 
                                           "25-29", "25-29", 
                                           "30-34", "30-34", 
                                           "35-39", "35-39", 
                                           "40-44", "40-44", 
                                           "45-49", "45-49", 
                                           
                                           "50-54", "50-54", "55-59", "55-59", "60-64", "60-64", 
                                           "65-69", "65-69", "70-74", "70-74", "75-79", "75-79", "80-84", 
                                           "80-84", "85-89", "85-89", "90+", "90+"), 
                       sex = c("M", "V", 
                                    "M", "V", "M", "V", "M", "V", "M", "V", "M", "V", "M", "V", "M", 
                                    "V", "M", "V", "M", "V", "M", "V", "M", "V", "M", "V", "M", "V", 
                                    "M", "V", "M", "V", "M", "V", "M", "V", "M", "V"), 
                       n_person = c(4254L, 4042L, 
                                           4104L, 3964L, 
                                           4030L, 3899L, 
                                           4689L, 5114L, 
                                           9004L, 11863L, 
                                           8936L, 9160L, 
                                           6603L, 6307L, 
                                           5340L, 5304L, 
                                           5043L, 4932L, 
                                           5244L, 5158L, 
                                           5550L, 5527L, 5454L, 5666L, 4899L, 5193L, 4388L, 4689L, 
                                           3736L, 4100L, 2312L, 2699L, 1511L, 2038L, 737L, 1344L, 308L, 
                                           710L)), row.names = c(NA, -38L), class = c("tbl_df", 
                                                                                      "tbl", "data.frame")) %>% 
  mutate(id = row_number())



data$time <- as.numeric(as.factor(data$age_grp))

data <- data %>%
  mutate(time = (time / 10) + 3) 

#### tree data ####
## add data_tree_end in the end to keep the branches in the animation
data_tree_end <- data %>% 
  mutate(time = time + 8,
         colr = sample(col_tree, nrow(data), replace = T)) %>% 
  mutate(n_person = ifelse(sex == "M", -1 * n_person, n_person))

data_tree <- data %>% 
  bind_rows(data %>% mutate(n_person = 0,
                            time = time - 3
                            )) %>% 
  mutate(colr = rep(c("skyblue1", "coral"), nrow(data))) %>% 
  mutate(n_person = ifelse(sex == "M", -1 * n_person, n_person)) %>% 
  bind_rows(data_tree_end)

#### balls start####

## getting boundaries of the barplot

height_min <- seq(from = 0.5, to = 18.5, by = 1)
height_max <- height_min + 1
left <- data %>% filter(sex == "M") %>% select(n_person) %>% mutate(n_person = -1 * n_person) %>% unlist()
right <- data %>% filter(sex == "V") %>% select(n_person) %>% unlist()

## almost random balls in tree within the limits

pos_balls <- list()

for (i in seq_along(height_min)) {
  
data_branch_left <- data.frame(x = runif(abs(ceiling(left[[i]] / 1000)), min = height_min[[i]], max = height_max[[i]]),
                             y = runif(abs(ceiling(left[[i]] / 1000)), min = left[[i]], max = 0))
  
data_branch_right <- data.frame(x = runif(abs(floor(right[[i]] / 1000)), min = height_min[[i]], max = height_max[[i]]),
                              y = runif(abs(floor(right[[i]] / 1000)), min = 0, max = right[[i]]))


pos_balls[[i]] <- bind_rows(data_branch_left, data_branch_right)
  
}

data_balls_start <- bind_rows(pos_balls)
data_balls_start <- data_balls_start %>% 
  mutate(time = runif(nrow(data_balls_start), -1, 5) + max(data_tree$time) - 1,
         colr = sample(col_balls, nrow(data_balls_start), replace = T),
         id = row_number() + 10000,
         sze = runif(nrow(data_balls_start), 2, 4))

#### balls iterations, change color and size ####

n_balls <- list()
t <- max(data_tree$time) + 2

for(i in 1:120){
  t <- t + 2
  
  balls_t <- data_balls_start %>% 
    mutate(time = runif(nrow(data_balls_start), 0, 2) + t,
           colr = sample(col_balls, nrow(data_balls_start), replace = T),
           sze = sze + runif(nrow(data_balls_start), -0.2, 0.2))
  
  n_balls[[i]] <- balls_t
  
}

## add it all together
data_balls <- data_balls_start %>% 
  bind_rows(n_balls)

#### label ####

label_1 <- data.frame(text = c("Fijne feestdagen en een"), stringsAsFactors = FALSE) %>% 
  mutate(time = 15)

label_1 <- label_1 %>% 
  bind_rows(label_1 %>% mutate(time = max(data_balls$time)))

text_1 <- c("Fijne feestdagen en een") 
text_2 <- c("significant gelukkig nieuwjaar! ")

#### add endtime for branches ####

data_tree <- data_tree %>% 
  bind_rows(data_tree_end %>% 
              mutate(time = max(data_balls$time)))

#### animation ####

plot <- ggplot() + 
  geom_col(data = data_tree, 
           aes(x = age_grp, 
               y = n_person, 
               fill = colr,
               group = id)) +
  scale_fill_identity() +
  geom_point(data = data_balls, aes(x, y, color = colr, group = id, size = sze)) +
  scale_color_identity() +
  coord_flip() +
  theme_void() +
  theme(legend.position = "none") +
  theme(plot.background = element_rect(fill = 'black', colour = NA), 
        panel.border = element_blank()) +
  geom_label(data = label_1, label = text_1, x = 22, y = 0, family = "mono", fontface = "bold", fill = "gold", size = 6.5) +
  geom_label(data = label_1, label = text_2, x = 20.5, y = 0, family = "mono", fontface = "bold", fill = "gold", size = 6.5)+
  expand_limits(x = c(0.5, 22.5), y = c(-12000, 12000)) +
  transition_components(time) +
  ### bouncy animation makes the transitions fun =).. it's ok for the baubles coulor changes and the tree itself
  ease_aes("bounce-out")

animate(plot, nframes = 2000)
anim_save("happy_holidays.gif")
```



See you in the next decade!
