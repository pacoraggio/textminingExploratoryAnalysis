# name: createData.R
# sbj: creating all data structures that will be 
#      used during the analysis and then loaded in the document
# type: script
# author: Paolo Coraggio
# create date: 11/03/2020

rm(list = ls())

options(stringsAsFactors = FALSE)

library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(tictoc)
library(patchwork)

source("createSampledText.R")
source("plotcustomf.R")
source("cleanCorpus.R")
source("wordfrequency.R")
source("tidyNgrams.R")

## Task 1: comparing the 1% of the data versus the whole

df.news <- createSampledDfText('./en_US/en_US.news.txt', 0.02, book = "news")
df.blogs <- createSampledDfText('./en_US/en_US.blogs.txt', 0.02, book = "blogs")
df.twitter <- createSampledDfText('./en_US/en_US.twitter.txt', 0.02, book = "twitter")

df.complete <- rbind(df.news, df.blogs, df.twitter)
df.complete$doc_id <- 1:nrow(df.complete)

# new clean.text removing websites references
tic()
df.twitter$text <- clean.text(df.twitter$text)
df.news$text <- clean.text(df.news$text)
df.blogs$text <- clean.text(df.blogs$text)
df.complete$text <- clean.text(df.complete$text)
toc()
# 15.25 sec elapsed - 10.08 sec elapsed -10.41 sec elapsed - 11.42 sec elapsed

df.twitter_withprofanity <- df.twitter
df.blogs_withprofanity <- df.blogs
df.news_withprofanity <- df.news
df.complete_withprofanity <- df.complete

save(df.news_withprofanity, file = "news_2percentWP.RData")
save(df.blogs_withprofanity, file = "blogs_2percentWP.RData")
save(df.twitter_withprofanity, file = "twitter_2percentWP.RData")
save(df.complete_withprofanity, file = "complete_2percentWP.RData")


# removing profanity
library(sentimentr)

profanity.list <- grep("^[a-z]",tolower(lexicon::profanity_alvarez), value = TRUE)
profanity.list <- grep("[a-z]$",profanity.list, value = TRUE)
profanity.list <- c(profanity.list, "bullshit", "bullshits", "motherfuckers")
profanity.df <- data.frame(word = profanity.list,
                              lexicon = "custom profanity list")

df.twitter$text <- removeWords(df.twitter$text, profanity.df$word)
df.blogs$text <- removeWords(df.blogs$text, profanity.df$word)
df.news$text <- removeWords(df.news$text, profanity.df$word)
df.complete$text <- removeWords(df.complete$text, profanity.df$word)

save(df.news, file = "dfnewsNP.RData")
save(df.blogs, file = "dfblogsNP.RData")
save(df.twitter, file = "dftwitterNP.RData")
save(df.complete, file = "dfcompleteNP.RData")

# a1 <- unique(tolower(lexicon::profanity_alvarez))
# grep("motherfucker", df.twitter$text, value = TRUE)
# grep("motherfucker", tw$text, value = TRUE)
# 
#        
# df.complete <- rbind(df.news,
#                      df.twitter,
#                      df.blogs)
# 
# news.profanityrate <- profanity(df.news$text, 
#                                     profanity_list = unique(tolower(lexicon::profanity_arr_bad)))
# 
# complete.profanityrate <- profanity(df.complete$text, 
#                                     profanity_list = unique(tolower(lexicon::profanity_alvarez)))
# 
# sum(news.profanityrate$profanity_count)
# 
# 
# 
# 
# sum(twitter.profanityrate$profanity_count) +
# sum(blogs.profanityrate$profanity_count) +
# sum(news.profanityrate$profanity_count)
# sum(blogsCleaned.profanityrate$profanity_count)
# 
# 
# 
# unique(twitter.profanityrate$profanity)
# str(twitter.profanityrate)
# 
# sum(complete.profanityrate$profanity_count)
# 
# head(twitter.profanityrate)
# sum(twitter.profanityrate$profanity_count)
# 
# 
# head(profanity.list)
# head(custom.stopwords)
# 
# 
# 
# df.blogs$text <- removeWords(df.blogs$text, profanity.list$word)
# 
# 
# ## Not run: 
# bw <- sample(unique(tolower(lexicon::profanity_arr_bad)), 4)
# mytext <- c(
#     sprintf('do you like this %s?  It is %s. But I hate really bad dogs', bw[1], bw[2]),
#     'I am the best friend.',
#     NA,
#     sprintf('I %s hate this %s', bw[3], bw[4]),
#     "Do you really like it?  I'm not happy"
# )
# 
# profanity(mytext, profanity_list = lexicon::profanity_arr_bad)

# twitter.profanityrate <- profanity(df.twitter$text, 
#           profanity_list = unique(tolower(lexicon::profanity_alvarez)))
# news.profanityrate <- profanity(df.news$text, 
#                                    profanity_list = unique(tolower(lexicon::profanity_alvarez)))
# blogs.profanityrate <- profanity(df.blogs$text, 
#                                    profanity_list = unique(tolower(lexicon::profanity_alvarez)))
# complete.profanityrate <- profanity(df.complete$text, 
#                                  profanity_list = unique(tolower(lexicon::complete.profanityrate)))

# profanity.list <- data.frame(word = lexicon::profanity_arr_bad,
#                              lexicon = "profanity_arr_bad")
# complete.profanityrate <- profanity(df.complete$text, 
#                                     profanity_list = unique(tolower(lexicon::profanity_arr_bad)))
# 
# sum(complete.profanityrate$profanity)
# tic()
# df.news$text <- removeWords(df.news$text, profanity.list$word)
# df.twitter$text <- removeWords(df.twitter$text, profanity.list$word)
# df.blogs$text <- removeWords(df.blogs$text, profanity.list$word)
# toc()
# profanity.list <- data.frame(word = lexicon::profanity_arr_bad,
#                              lexicon = "profanity_arr_bad")
# profanity.lista <- data.frame(word = a1,
#                               lexicon = "profanity_a1")
# profanity.listalvares <- data.frame(word = tolower(lexicon::profanity_alvarez),
#                              lexicon = "profanity_alvarez")
# 
# df.twitter$text <-removeWords(df.twitter$text, profanity.lista$word)
# grep("shit", df.twitter$text, value = TRUE)

