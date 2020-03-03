# name: tmEDAScript
# sbj: tidyverseApproach
# author: Paolo Coraggio
# create date: 02/03/2020

rm(list = ls())

options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL', 'C')

library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(tictoc)

source("createSampledText.R")

a1 <- read.table('./en_US/summary.txt', 
                 col.names = c("lines",
                               "words",
                               "characters",
                               "bytes",
                               "source"),
                 nrows = 3)

a1 <- mutate(a1, 
             Mb = round(bytes*0.00000095367432,2))
    
a1 <- a1[,c(5,1,2,3,6)]


# en_US.blogs.txt made of 899288 lines
# en_US.twitter.txt made of 2360148 lines
# en_US.news.txt made of 1010242 lines

tic()
sample.news <- createSampledText('./en_US/en_US.news.txt', 0.05)
sample.blogs <- createSampledText('./en_US/en_US.blogs.txt', 0.05)
sample.twitter <- createSampledText('./en_US/en_US.twitter.txt', 0.05)
toc()

as.integer(899288 * 0.05)
as.integer(2360148 * 0.05)
as.integer(1010242 * 0.05)

df.news <- data.frame(n_row = 1:length(sample.news),
                       text = sample.news, stringsAsFactors = FALSE)

df.blogs <- data.frame(n_row = 1:length(sample.blogs),
                      text = sample.blogs, stringsAsFactors = FALSE)

df.twitter <- data.frame(n_row = 1:length(sample.twitter),
                      text = sample.twitter, stringsAsFactors = FALSE)

tic()
tidyNews <- df.news %>% 
    unnest_tokens(word, text) %>%
    anti_join(stop_words)

tidyBlogs <- df.blogs %>% 
    unnest_tokens(word, text) %>%
    anti_join(stop_words)

tidyTwitter <- df.twitter %>% 
    unnest_tokens(word, text) %>%
    anti_join(stop_words)
toc()

tidyNews %>% 
    count(word, sort = TRUE)

tidyBlogs %>% 
    count(word, sort = TRUE)

remove_reg <- "&amp;|&lt|&gt;"
tidyTwitter <- df.twitter %>% 
    filter(!str_detect(text, "^RT")) %>%
    mutate(text = str_remove_all(text, remove_reg)) %>%
    unnest_tokens(word, text, token = "tweets") %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words, "'"),
           str_detect(word,"[a-z]"))

l2 <- df.twitter[str_detect(df.twitter$text, "^RT"),]$text

head(l3)
head(l3)
    
tidyTwitter %>% 
    count(word, sort = TRUE)


length(grep("sorry", df.twitter$text, ignore.case = TRUE))
count(tidyTwitter[tidyTwitter$word == "sorry",])

stop_words[stop_words$word == "sorry",]