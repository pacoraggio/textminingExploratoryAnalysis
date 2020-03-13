# name: wordfrequencyAnalysis.R
# sbj: word frequency analysis
#      different Datasets
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

load("dfnewsNP.RData")
load("dfblogsNP.RData")
load("dftwitterNP.RData")
load("dfcompleteNP.RData")

load("news_2percentWP.RData")
load("blogs_2percentWP.RData")
load("twitter_2percentWP.RData")
load("complete_2percentWP.RData")

# Word frequency with profanity
tic()
df.news_wfP <- word.frequency(df.news_withprofanity)
df.blogs_wfP <- word.frequency(df.blogs_withprofanity)
df.twitter_wfP <- word.frequency(df.twitter_withprofanity)
df.complete_wfP <- word.frequency(df.complete_withprofanity)
toc()
# 9.13 sec elapsed

# Word frequency without profanity
tic()
df.news_wf <- word.frequency(df.news)
df.blogs_wf <- word.frequency(df.blogs)
df.twitter_wf <- word.frequency(df.twitter)
df.complete_wf <- word.frequency(df.complete)
toc()
# 7.41 sec elapsed

# profanity rate
(nrow(df.news_wfP) - nrow(df.news_wf))/(nrow(df.news_wfP))
(nrow(df.blogs_wfP) - nrow(df.blogs_wf))/(nrow(df.blogs_wfP))

df.news_prof <- df.news_wfP[!(df.news_wfP$word %in% df.news_wf$word),]
df.blogs_prof <- df.blogs_wfP[!(df.blogs_wfP$word %in% df.blogs_wf$word),]
df.twitter_prof <- df.twitter_wfP[!(df.twitter_wfP$word %in% df.twitter_wf$word),]
df.complete_prof <- df.complete_wfP[!(df.complete_wfP$word %in% df.complete_wf$word),]

head(df.news_prof)
news.sumprof <- sum(df.news_prof$frequency)
blogs.sumprof <- sum(df.blogs_prof$frequency)
twitter.sumprof <- sum(df.twitter_prof$frequency)
complete.sumprof <- sum(df.complete_prof$frequency)

news.sumwords <- sum(df.news_wfP$frequency)
blogs.sumwords <- sum(df.blogs_wfP$frequency)
twitter.sumwords <- sum(df.twitter_wfP$frequency)
complete.sumwords <- sum(df.complete_wfP$frequency)

news.sumprof + blogs.sumprof + twitter.sumprof
complete.sumprof

news.sumprof/news.sumwords
blogs.sumprof/blogs.sumwords
twitter.sumprof/twitter.sumwords

complete.sumprof/complete.sumwords

head(df.complete_wf)
windows()
windows()
ggplot(df.complete_wf, aes(x = word, y = frequency)) +
           geom_bar(stat = "identity", fill = "darkred")

accept <- df.complete_wf[df.complete_wf$frequency > 10,]
accept <- df.complete_wf[nchar(df.complete_wf$word) < 25,]

t.news <- df.news_wf[df.news_wf$frequency > 10,]
t.blogs <- df.blogs_wf[df.blogs_wf$frequency > 10,]
t.twitter <- df.twitter_wf[df.twitter_wf$frequency > 10,]

t.news <- t.news[nchar(t.news$word) < 25,]
t.blogs <- t.blogs[nchar(t.blogs$word) < 25,]
t.twitter <- t.twitter[nchar(t.twitter$word) < 25,]

r.news <- t.news[with(t.news, order(nchar(t.news$word),decreasing = TRUE)),]
r.blogs <- t.blogs[with(t.blogs, order(nchar(t.blogs$word),decreasing = TRUE)),]
r.twitter <- t.twitter[with(t.twitter, order(nchar(t.twitter$word),decreasing = TRUE)),]

head(r.news, n = 20)
head(r.blogs, n = 20)
head(r.twitter, n = 20)

news.wf[nchar(news.wf$word) == max(nchar(news.wf$word)),]$word
nwww <- grep("www", df.news$text, value = TRUE)
gwww <- grep("www", df.blogs$text, value = TRUE)
twww <- grep("www", df.twitter$text, value = TRUE)
grep("http", blogs.wf$word, value = TRUE)
# 8.44 sec elapsed - 8.66 sec elapsed - 7.28
nrow(news.wf)
nrow(twitter.wf)
nrow(blogs.wf)

## mean character lenght for each dataset

df.charstat <- data.frame("max length" = c(max(nchar(news.wf$word)),
                                            max(nchar(blogs.wf$word)),
                                            max(nchar(twitter.wf$word)),
                                            max(nchar(complete.wf$word))),
                          "mean" = c(mean(nchar(news.wf$word)),
                                      mean(nchar(blogs.wf$word)),
                                      mean(nchar(twitter.wf$word)),
                                      mean(nchar(complete.wf$word))),
                          "sd" = c(sd(nchar(news.wf$word)),
                                    sd(nchar(blogs.wf$word)),
                                    sd(nchar(twitter.wf$word)),
                                    sd(nchar(complete.wf$word))),
                          row.names = c("news", "blogs", "twitter", "complete")
                          )

blogs.wf[nchar(blogs.wf$word) == max(nchar(blogs.wf$word)),]$word
twitter.wf[nchar(twitter.wf$word) == max(nchar(twitter.wf$word)),]$word
news.wf[nchar(news.wf$word) == max(nchar(news.wf$word)),]$word
# "wwwdnrstatemiusspatialdatalibrarypdfmapsmineralleaseinformationoaklandnominationspdf"

wwwcomplete <- grep("^www", df.complete$text, value = TRUE)
wwwnews <- grep("^www", df.news$text, value = TRUE)


tic()
news.wfSW <- word.frequency(df.news, remove.stopwords = TRUE)
blogs.wfSW <- word.frequency(df.blogs, remove.stopwords = TRUE)
twitter.wfSW <- word.frequency(df.twitter, remove.stopwords = TRUE)
complete.wfSW <- word.frequency(df.complete, remove.stopwords = TRUE)
toc()

head(news.wf)
# 8 sec elapsed - 7.92 sec elapsed

r2 <- grep("shit", complete.wfSW$word, value = TRUE)
length(r2)

# 42 occurrences without tyding

tail(complete.wf$frequency)
summary(complete.wf$frequency)
nrow(complete.wf)
nrow(complete.wf[complete.wf$frequency == 1,])

nrow(complete.wf[complete.wf$frequency < 10,])/nrow(complete.wf)
sum(news.wfSW$frequency)

source("plotcustomf.R")
windows()
pnews <- plotbar.wf(news.wf, title = "News Word Frequency", topn = 15)
pblogs <- plotbar.ngramf(blogs.wf, title = "Blogs Word Frequency", topn = 15)
ptwitter <- plotbar.ngramf(twitter.wf, title = "Twitter Word Frequency", topn = 15)
pcomplete <- plotbar.ngramf(complete.wf, title = "Complete Word Frequency", topn = 15)

pnewsSW <- plotbar.ngramf(news.wfSW, title = "News Word Frequency (SW)", topn = 15)
pblogsSW <- plotbar.ngramf(blogs.wfSW, title = "Blogs Word Frequency (SW)", topn = 15)
ptwitterSW <- plotbar.ngramf(twitter.wfSW, title = "Twitter Word Frequency (SW)" , topn = 15)
pcompleteSW <- plotbar.ngramf(complete.wfSW, title = "Complete Word Frequency (SW)", topn = 15)

windows()
pnews + pnewsSW
windows()
pcomplete + pcompleteSW

news.wf[news.wf$word == "sex",]$frequency +
twitter.wf[twitter.wf$word == "sex",]$frequency +
blogs.wf[blogs.wf$word == "sex",]$frequency ==
complete.wf[complete.wf$word == "sex",]$frequency


## after profanity filter

load("dfnewsNP.RData")
load("dfblogsNP.RData")
load("dftwitterNP.RData")
load("dfcompleteNP.RData")

