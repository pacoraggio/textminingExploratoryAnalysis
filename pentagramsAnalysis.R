# name: pentagramsAnalysis.R
# sbj: pentagrams frequency analysis
#      different Datasets
# type: script
# author: Paolo Coraggio
# create date: 11/03/2020

rm(list = ls())

options(stringsAsFactors = FALSE)

library(dplyr)
library(tidytext)
library(janeaustenr)
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
news.pentagrams <- create.pentagramsfreq(df.news)
blogs.pentagrams <- create.pentagramsfreq(df.blogs)
twitter.pentagrams <- create.pentagramsfreq(df.twitter)
complete.pentagrams <- create.pentagramsfreq(df.complete)
toc()
# 58.27 sec elapsed - 58.1 sec elapsed - 105.9 sec elapsed

tic()
news.pentagramsSW <- create.pentagramsfreq(df.news, remove.stopwords = TRUE)
blogs.pentagramsSW <- create.pentagramsfreq(df.blogs, remove.stopwords = TRUE)
twitter.pentagramsSW <- create.pentagramsfreq(df.twitter, remove.stopwords = TRUE)
complete.pentagramsSW <- create.pentagramsfreq(df.complete, remove.stopwords = TRUE)
toc()
# 20.44 sec elapsed - 19.58 sec elapsed - 35.31 sec elapsed

save(news.pentagrams, file = "news_pentagrams.RData")
save(blogs.pentagrams, file = "blogs_pentagrams.RData")
save(twitter.pentagrams, file = "twitter_pentagrams.RData")
save(complete.pentagrams, file = "complete_pentagrams.RData")

save(news.pentagramsSW, file = "news_pentagramsSW.RData")
save(blogs.pentagramsSW, file = "blogs_pentagramsSW.RData")
save(twitter.pentagramsSW, file = "twitter_pentagramsSW.RData")
save(complete.pentagramsSW, file = "complete_pentagramsSW.RData")

pnews.pentagrams <- plotbar.ngramf(news.pentagrams, title = "News pentagrams Frequency", topn = 15, gram = 5)
pblogs.pentagrams <- plotbar.ngramf(blogs.pentagrams, title = "Blogs pentagrams Frequency", topn = 15, gram = 5)
ptwitter.pentagrams <- plotbar.ngramf(twitter.pentagrams, title = "Twitter pentagrams Frequency", topn = 15, gram = 5)
pcomplete.pentagrams <- plotbar.ngramf(complete.pentagrams, title = "Complete pentagrams Frequency", topn = 15, gram = 5)

pnews.pentagramsSW <- plotbar.ngramf(news.pentagramsSW, title = "News pentagrams Frequency", topn = 15, gram = 5)
pblogs.pentagramsSW <- plotbar.ngramf(blogs.pentagramsSW, title = "Blogs pentagrams Frequency", topn = 15, gram = 5)
ptwitter.pentagramsSW <- plotbar.ngramf(twitter.pentagramsSW, title = "Twitter pentagrams Frequency", topn = 15, gram = 5)
pcomplete.pentagramsSW <- plotbar.ngramf(complete.pentagramsSW, title = "Complete pentagrams Frequency", topn = 15, gram = 5)

save(pnews.pentagrams, file = "pnews_pentagrams.RData")
save(pnews.pentagramsSW, file = "pnews_pentagramsSW.RData")
save(pblogs.pentagrams, file = "pblogs_pentagrams.RData")
save(pblogs.pentagramsSW, file = "pblogs_pentagramsSW.RData")
save(ptwitter.pentagrams, file = "ptwitter_pentagrams.RData")
save(ptwitter.pentagramsSW, file = "ptwitter_pentagramsSW.RData")
save(pcomplete.pentagrams, file = "pcomplete_pentagrams.RData")
save(pcomplete.pentagramsSW, file = "pcomplete_pentagramsSW.RData")

windows()
pnews.pentagrams + pnews.pentagramsSW
windows()
pblogs.pentagrams + pblogs.pentagramsSW
windows()
ptwitter.pentagrams + ptwitter.pentagramsSW
windows()
pcomplete.pentagrams + pcomplete.pentagramsSW
head(complete.pentagrams)
tail(complete.pentagrams[complete.pentagrams$n > 1,])
nrow(complete.pentagrams[complete.pentagrams$n > 1,])
nrow(complete.pentagrams[complete.pentagrams$n > 1,])/nrow(complete.pentagrams)
