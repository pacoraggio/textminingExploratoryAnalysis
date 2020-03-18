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
names(complete.trigrams)
length(unique(complete.trigrams$trigram))
nrow(complete.trigrams[complete.trigrams$n > 5,])
tail(complete.trigrams[complete.trigrams$n > 5,])

nrow(complete.trigrams[complete.trigrams$n > 5,])/nrow(complete.trigrams)
windows()
ggplot(complete.trigrams[complete.trigrams$n > 5,], aes(n)) +
    geom_histogram(bins = 400)
