# name: tetragramsAnalysis.R
# sbj: tetragrams frequency analysis
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
news.tetragrams <- create.tetragramsfreq(df.news)
blogs.tetragrams <- create.tetragramsfreq(df.blogs)
twitter.tetragrams <- create.tetragramsfreq(df.twitter)
complete.tetragrams <- create.tetragramsfreq(df.complete)
toc()
# 48.8 sec elapsed - 45.69 sec elapsed - 55.51 sec elapsed

tic()
news.tetragramsSW <- create.tetragramsfreq(df.news, remove.stopwords = TRUE)
blogs.tetragramsSW <- create.tetragramsfreq(df.blogs, remove.stopwords = TRUE)
twitter.tetragramsSW <- create.tetragramsfreq(df.twitter, remove.stopwords = TRUE)
complete.tetragramsSW <- create.tetragramsfreq(df.complete, remove.stopwords = TRUE)
toc()
# 19.48 sec elapsed - 46.16 sec elapsed - 31.2 sec elapsed - 45.2 sec elapsed

save(news.tetragrams, file = "news_tetragrams.RData")
save(blogs.tetragrams, file = "blogs_tetragrams.RData")
save(twitter.tetragrams, file = "twitter_tetragrams.RData")
save(complete.tetragrams, file = "complete_tetragrams.RData")

save(news.tetragramsSW, file = "news_tetragramsSW.RData")
save(blogs.tetragramsSW, file = "blogs_tetragramsSW.RData")
save(twitter.tetragramsSW, file = "twitter_tetragramsSW.RData")
save(complete.tetragramsSW, file = "complete_tetragramsSW.RData")



pnews.tetragrams <- plotbar.ngramf(news.tetragrams, title = "News Tetragrams Frequency", topn = 15, gram = 4)
pblogs.tetragrams <- plotbar.ngramf(blogs.tetragrams, title = "Blogs Tetragrams Frequency", topn = 15, gram = 4)
ptwitter.tetragrams <- plotbar.ngramf(twitter.tetragrams, title = "Twitter Tetragrams Frequency", topn = 15, gram = 4)
pcomplete.tetragrams <- plotbar.ngramf(complete.tetragrams, title = "Complete Tetragrams Frequency", topn = 15, gram = 4)

pnews.tetragramsSW <- plotbar.ngramf(news.tetragramsSW, title = "News Tetragrams Frequency", topn = 15, gram = 4)
pblogs.tetragramsSW <- plotbar.ngramf(blogs.tetragramsSW, title = "Blogs Tetragrams Frequency", topn = 15, gram = 4)
ptwitter.tetragramsSW <- plotbar.ngramf(twitter.tetragramsSW, title = "Twitter Tetragrams Frequency", topn = 15, gram = 4)
pcomplete.tetragramsSW <- plotbar.ngramf(complete.tetragramsSW, title = "Complete Tetragrams Frequency", topn = 15, gram = 4)

save(pnews.tetragrams, file = "pnews_tetragrams.RData")
save(pnews.tetragramsSW, file = "pnews_tetragramsSW.RData")
save(pblogs.tetragrams, file = "pblogs_tetragrams.RData")
save(pblogs.tetragramsSW, file = "pblogs_tetragramsSW.RData")
save(ptwitter.tetragrams, file = "ptwitter_tetragrams.RData")
save(ptwitter.tetragramsSW, file = "ptwitter_tetragramsSW.RData")
save(pcomplete.tetragrams, file = "pcomplete.tetragrams.RData")
save(pcomplete.tetragramsSW, file = "pcomplete.tetragramsSW.RData")

windows()
pnews.tetragrams + pnews.tetragramsSW
windows()
pblogs.tetragrams + pblogs.tetragramsSW
windows()
ptwitter.tetragrams + ptwitter.tetragramsSW
windows()
pcomplete.tetragrams + pcomplete.tetragramsSW

tail(complete.tetragrams[complete.tetragrams$n > 3,])
nrow(complete.tetragrams)
nrow(complete.tetragrams[complete.tetragrams$n > 3,])

sum(complete.tetragrams$n)
sum(complete.tetragrams[complete.tetragrams$n <= 3,]$n)
tail(complete.tetragrams[complete.tetragrams$n > 3,])
sum(complete.tetragrams[complete.tetragrams$n > 3,]$n)
head(complete.tetragrams)
