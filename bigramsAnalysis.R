# name: bigramsAnalysis.R
# sbj: bigrams frequency analysis
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
news.bigrams <- create.bigramsfreq(df.news)
blogs.bigrams <- create.bigramsfreq(df.blogs)
twitter.bigrams <- create.bigramsfreq(df.twitter)
complete.bigrams <- create.bigramsfreq(df.complete)
toc()
# 18.55 sec elapsed - 21.31 sec elapsed
save(news.bigrams, file = "news_bigrams.RData")
save(blogs.bigrams, file = "blogs_bigrams.RData")
save(twitter.bigrams, file = "twitter_bigrams.RData")
save(complete.bigrams, file = "complete_bigrams.RData")

tic()
news.bigramsSW <- create.bigramsfreq(df.news, remove.stopwords = TRUE)
blogs.bigramsSW <- create.bigramsfreq(df.blogs, remove.stopwords = TRUE)
twitter.bigramsSW <- create.bigramsfreq(df.twitter, remove.stopwords = TRUE)
complete.bigramsSW <- create.bigramsfreq(df.complete, remove.stopwords = TRUE)
toc()

save(news.bigramsSW, file = "news_bigramsSW.RData")
save(blogs.bigramsSW, file = "blogs_bigramsSW.RData")
save(twitter.bigramsSW, file = "twitter_bigramsSW.RData")
save(complete.bigramsSW, file = "complete_bigramsSW.RData")

# 17.39 sec elapsed - 15.53 sec elapsed

pbigramsnews <- plotbar.ngramf(news.bigrams, title = "News Bigrams Frequency", topn = 15, gram = 2)
pbigramsblogs <- plotbar.ngramf(blogs.bigrams, title = "Blogs Bigrams Frequency", topn = 15, gram = 2)
pbigramstwitter <- plotbar.ngramf(twitter.bigrams, title = "Twitter Bigrams Frequency", topn = 15, gram = 2)
pbigramscomplete <- plotbar.ngramf(complete.bigrams, title = "Complete Bigrams Frequency", topn = 15, gram = 2)

pbigramsnewsSW <- plotbar.ngramf(news.bigramsSW, title = "News Bigrams Frequency", topn = 15, gram = 2)
pbigramsblogsSW <- plotbar.ngramf(blogs.bigramsSW, title = "Blogs Bigrams Frequency", topn = 15, gram = 2)
pbigramstwitterSW <- plotbar.ngramf(twitter.bigramsSW, title = "Twitter Bigrams Frequency", topn = 15, gram = 2)
pbigramscompleteSW <- plotbar.ngramf(complete.bigramsSW, title = "Complete Bigrams Frequency", topn = 15, gram = 2)

save(pbigramsnews, file = "pbigramsnews.RData")
save(pbigramsblogs, file = "pbigramsblogs.RData")
save(pbigramstwitter, file = "pbigramstwitter.RData")
save(pbigramscomplete, file = "pbigramscomplete.RData")

save(pbigramsnewsSW, file = "pbigramsnewsSW.RData")
save(pbigramsblogsSW, file = "pbigramsblogsSW.RData")
save(pbigramstwitterSW, file = "pbigramstwitterSW.RData")
save(pbigramscompleteSW, file = "pbigramscompleteSW.RData")

windows()
pbigramsnews + pbigramsnewsSW
windows()
pbigramsblogs + pbigramsblogsSW

windows()
pbigramstwitter + pbigramstwitterSW

windows()
pbigramscomplete + pbigramscompleteSW

####

load("news_bigrams.RData")
load("news_bigramsSW.RData")
load("blogs_bigrams.RData")
load("blogs_bigramsSW.RData")
load("twitter_bigrams.RData")
load("twitter_bigramsSW.RData")
load("complete_bigrams.RData")
load("complete_bigramsSW.RData")

tail(complete.bigrams[complete.bigrams$n > 6,])
tail(twitter.bigrams[twitter.bigrams$n > 2,])
tail(twitter.bigramsSW[twitter.bigramsSW$n > 2,])