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

load("news_2percent.RData")
load("blogs_2percent.RData")
load("twitter_2percent.RData")
load("complete_2percent.RData")

tic()
news.bigrams <- create.bigramsfreq(df.news)
blogs.bigrams <- create.bigramsfreq(df.blogs)
twitter.bigrams <- create.bigramsfreq(df.twitter)
complete.bigrams <- create.bigramsfreq(df.complete)
toc()

# 18.55 sec elapsed

tic()
news.bigramsSW <- create.bigramsfreq(df.news, remove.stopwords = TRUE)
blogs.bigramsSW <- create.bigramsfreq(df.blogs, remove.stopwords = TRUE)
twitter.bigramsSW <- create.bigramsfreq(df.twitter, remove.stopwords = TRUE)
complete.bigramsSW <- create.bigramsfreq(df.complete, remove.stopwords = TRUE)
toc()

# 17.39 sec elapsed

pnews <- plotbar.ngramf(news.bigrams, title = "News Bigrams Frequency", topn = 15, gram = 2)
pblogs <- plotbar.ngramf(blogs.bigrams, title = "Blogs Bigrams Frequency", topn = 15, gram = 2)
ptwitter <- plotbar.ngramf(twitter.bigrams, title = "Twitter Bigrams Frequency", topn = 15, gram = 2)
pcomplete <- plotbar.ngramf(complete.bigrams, title = "Complete Bigrams Frequency", topn = 15, gram = 2)

pnewsSW <- plotbar.ngramf(news.bigramsSW, title = "News Bigrams Frequency", topn = 15, gram = 2)
pblogsSW <- plotbar.ngramf(blogs.bigramsSW, title = "Blogs Bigrams Frequency", topn = 15, gram = 2)
ptwitterSW <- plotbar.ngramf(twitter.bigramsSW, title = "Twitter Bigrams Frequency", topn = 15, gram = 2)
pcompleteSW <- plotbar.ngramf(complete.bigramsSW, title = "Complete Bigrams Frequency", topn = 15, gram = 2)

windows()
pnews + pnewsSW
windows()
pcomplete + pcompleteSW