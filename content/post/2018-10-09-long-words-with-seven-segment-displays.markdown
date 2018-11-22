---
title: Long words with Seven-Segment Displays
author: S Dikkerboom
date: '2018-10-09'
slug: long-words-with-seven-segment-displays
categories:
  - R
  - strings
  - seven-segment displays
tags:
  - R
  - strings
  - seven-segment displays
  - Tom Scott
  - words
---

One of the most entertaining things you could do at primary school was spelling words with the seven-segment calculator. A little while ago Tom Scott did a video about seven-segment displays. You can see it here:

<iframe width="560" height="315" src="https://www.youtube.com/embed/zp4BMR88260" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>

<br></br>
This is exactly the quick and fun exercise i need to start my blog. And i think R (plus some packages) is especially suited for this task. So, what are the longest words we can make with the limitations of a seven-segment display? Let's find out! 

First we need some data. Let's use the same list as in the video (source: https://github.com/dwyl/english-words):






```r
library(tidyverse)
words <- read.delim("https://raw.githubusercontent.com/dwyl/english-words/master/words.txt", 
                    stringsAsFactors = FALSE)
```


The `stringsAsFactors` is needed, otherwise R turns the words into factors. And some functions we want to use later don't work on factors. 

Now we have a list of 466543 words! 

Let's see what the longest words are. We will stick to the same limitations as in the video. So we will filter out words with the letters g, k, m, q, v, w, x and z. With the help of Tidyverse, this code chunk is only 4 lines long:



```r
length_words <- words %>% 
  filter(!grepl("[gkmqvwxzGKMQVWXZ]", X2)) %>% 
  mutate(length = nchar(X2)) %>% 
  arrange(desc(length))
```

In the second line we filter for words with the letters that are not allowed. With the `!` we get the opposite of the following statement. So we throw away the words containing the letters that we are filtering for. To be sure we also check for the capital letters. In the third line we count the length of all the remaining words. And finally we arrange the words from longest to shortest.

Now let's see the result:


```r
head(length_words, 10)
```

```
##                                 X2 length
## 1  dichlorodiphenyltrichloroethane     31
## 2         bras-dessus-bras-dessous     24
## 3         pseudointernationalistic     24
## 4         scientificophilosophical     24
## 5         tetraiodophenolphthalein     24
## 6          analytico-architectural     23
## 7          chlorotrifluoroethylene     23
## 8          electro-ultrafiltration     23
## 9          phenolsulphonephthalein     23
## 10         polytetrafluoroethylene     23
```


Well... 
Except for this enjoyable exercise I've learned that a lot of the longer words in this list are almost unpronounceable =).

<br></br>

