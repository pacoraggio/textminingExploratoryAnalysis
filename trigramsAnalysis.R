# name: trigramsAnalysis.R
# sbj: trigrams frequency analysis
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

load("news_2percentNoProf.RData")
load("blogs_2percentNoProf.RData")
load("twitter_2percentNoProf.RData")
load("complete_2PercentNoProf.RData")

tic()
news.trigrams <- create.trigramsfreq(df.news)
blogs.trigrams <- create.trigramsfreq(df.blogs)
twitter.trigrams <- create.trigramsfreq(df.twitter)
complete.trigrams <- create.trigramsfreq(df.complete)
toc()
# 36.1 sec elapsed - 33.13 sec elapsed - 36.17 sec elapsed

tic()
news.trigramsSW <- create.trigramsfreq(df.news, remove.stopwords = TRUE)
blogs.trigramsSW <- create.trigramsfreq(df.blogs, remove.stopwords = TRUE)
twitter.trigramsSW <- create.trigramsfreq(df.twitter, remove.stopwords = TRUE)
complete.trigramsSW <- create.trigramsfreq(df.complete, remove.stopwords = TRUE)
toc()
# 43.14 sec elapsed - 18.86 sec elapsed - 19.2 sec elapsed

save(news.trigrams, file = "news_trigrams.RData")
save(blogs.trigrams, file = "blogs_trigrams.RData")
save(twitter.trigrams, file = "twitter_trigrams.RData")
save(complete.trigrams, file = "complete_trigrams.RData")

save(news.trigramsSW, file = "news_trigramsSW.RData")
save(blogs.trigramsSW, file = "blogs_trigramsSW.RData")
save(twitter.trigramsSW, file = "twitter_trigramsSW.RData")
save(complete.trigramsSW, file = "complete_trigramsSW.RData")

pnews.trigrams <- plotbar.ngramf(news.trigrams, title = "News Trigrams Frequency", topn = 15, gram = 3)
pblogs.trigrams <- plotbar.ngramf(blogs.trigrams, title = "Blogs Trigrams Frequency", topn = 15, gram = 3)
ptwitter.trigrams <- plotbar.ngramf(twitter.trigrams, title = "Twitter Trigrams Frequency", topn = 15, gram = 3)
pcomplete.trigrams <- plotbar.ngramf(complete.trigrams, title = "Complete Trigrams Frequency", topn = 15, gram = 3)

pnews.trigramsSW <- plotbar.ngramf(news.trigramsSW, title = "News Trigrams Frequency", topn = 15, gram = 3)
pblogs.trigramsSW <- plotbar.ngramf(blogs.trigramsSW, title = "Blogs Trigrams Frequency", topn = 15, gram = 3)
ptwitter.trigramsSW <- plotbar.ngramf(twitter.trigramsSW, title = "Twitter Trigrams Frequency", topn = 15, gram = 3)
pcomplete.trigramsSW <- plotbar.ngramf(complete.trigramsSW, title = "Complete Trigrams Frequency", topn = 15, gram = 3)

save(pnews.trigrams, file = "pnews_trigrams.RData")
save(pnews.trigramsSW, file = "pnews_trigramsSW.RData")
save(pblogs.trigrams, file = "pblogs_trigrams.RData")
save(pblogs.trigramsSW, file = "pblogs_trigramsSW.RData")
save(ptwitter.trigrams, file = "ptwitter_trigrams.RData")
save(ptwitter.trigramsSW, file = "ptwitter_trigramsSW.RData")
save(pcomplete.trigrams, file = "pcomplete.trigrams.RData")
save(pcomplete.trigramsSW, file = "pcomplete.trigramsSW.RData")

windows()
pnews.trigrams + pnews.trigramsSW
windows()
pblogs.trigrams + pblogs.trigramsSW
windows()
ptwitter.trigrams + ptwitter.trigramsSW
windows()
pcomplete.trigrams + pcomplete.trigramsSW

### head and tail
rm(list = ls())
library(ggplot2)

load("news_trigrams.RData")
load("blogs_trigrams.RData")
load("twitter_trigrams.RData")
load("complete_trigrams.RData")

names(complete.trigrams)

nrow(news.trigrams[news.trigrams$n == 1,])
nrow(blogs.trigrams[blogs.trigrams$n == 1,])
nrow(twitter.trigrams[twitter.trigrams$n == 1,])
nrow(complete.trigrams[complete.trigrams$n == 1,])

nrow(news.trigrams[news.trigrams$n == 1,])/nrow(news.trigrams)
nrow(blogs.trigrams[blogs.trigrams$n == 1,])/nrow(blogs.trigrams)
nrow(twitter.trigrams[twitter.trigrams$n == 1,])/nrow(twitter.trigrams)
nrow(complete.trigrams[complete.trigrams$n == 1,])/nrow(complete.trigrams)

news.percenttrigrams <- c()
blogs.percenttrigrams <- c()
twitter.percenttrigrams <- c()
complete.percenttrigrams <- c()

k1 <- 1:30

for(i in k1)
{
    news.percenttrigrams <- c(news.percenttrigrams,
                             nrow(news.trigrams[news.trigrams$n <= i,])/nrow(news.trigrams))
    blogs.percenttrigrams <- c(blogs.percenttrigrams,
                              nrow(blogs.trigrams[blogs.trigrams$n <= i,])/nrow(blogs.trigrams))
    twitter.percenttrigrams <- c(twitter.percenttrigrams,
                                nrow(twitter.trigrams[twitter.trigrams$n <= i,])/nrow(twitter.trigrams))
    complete.percenttrigrams <- c(complete.percenttrigrams,
                                 nrow(complete.trigrams[complete.trigrams$n <= i,])/nrow(complete.trigrams))
}


df.newsfreqpercenttri <- data.frame(frequency = k1,
                                 percentage = news.percenttrigrams,
                                 type = rep("news 3-grams", length(k1)))
df.blogsfreqpercenttri <- data.frame(frequency = k1,
                                  percentage = blogs.percenttrigrams,
                                  type = rep("blogs 3-grams", length(k1)))
df.twitterfreqpercenttri <- data.frame(frequency = k1,
                                    percentage = twitter.percenttrigrams,
                                    type = rep("twitter 3-grams", length(k1)))
df.completefreqpercenttri <- data.frame(frequency = k1,
                                     percentage = complete.percenttrigrams,
                                     type = rep("complete 3-grams", length(k1)))

df.freqpercenttri <-rbind(df.newsfreqpercenttri,
                       df.blogsfreqpercenttri,
                       df.twitterfreqpercenttri,
                       df.completefreqpercenttri)

df.freqpercenttri$type = factor(df.freqpercenttri$type, levels = c("news 3-grams",
                                                                   "blogs 3-grams",
                                                                   "twitter 3-grams", 
                                                                   "complete 3-grams"))

library(ggthemes)
windows()
p.freqpercenttri <- ggplot(df.freqpercenttri, aes(frequency, percentage, color = type)) +
    geom_point(size = 0.8) +
    theme_solarized_2(base_size = 12) 

save(df.freqpercenttri, file = "df_freqpercenttri.RData")
save(p.freqpercenttri, file = "plot_freqpercenttri.RData")
