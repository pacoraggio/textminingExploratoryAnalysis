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

# load("news_2percent.RData")
# load("blogs_2percent.RData")
# load("twitter_2percent.RData")
# load("complete_2percent.RData")

load("dfnewsNP.RData")
load("dfblogsNP.RData")
load("dftwitterNP.RData")
load("dfcompleteNP.RData")

tic()
news.trigrams <- create.trigramsfreq(df.news)
blogs.trigrams <- create.trigramsfreq(df.blogs)
twitter.trigrams <- create.trigramsfreq(df.twitter)
complete.trigrams <- create.trigramsfreq(df.complete)
toc()
# without restarting R 36.1 sec elapsed
# 70.91 sec elapsed

tic()
news.trigramsSW <- create.trigramsfreq(df.news, remove.stopwords = TRUE)
blogs.trigramsSW <- create.trigramsfreq(df.blogs, remove.stopwords = TRUE)
twitter.trigramsSW <- create.trigramsfreq(df.twitter, remove.stopwords = TRUE)
complete.trigramsSW <- create.trigramsfreq(df.complete, remove.stopwords = TRUE)
toc()
# without restarting R 43.14 sec elapsed

pnews.trigrams <- plotbar.ngramf(news.trigrams, title = "News Trigrams Frequency", topn = 15, gram = 3)
pblogs.trigrams <- plotbar.ngramf(blogs.trigrams, title = "Blogs Trigrams Frequency", topn = 15, gram = 3)
ptwitter.trigrams <- plotbar.ngramf(twitter.trigrams, title = "Twitter Trigrams Frequency", topn = 15, gram = 3)
pcomplete.trigrams <- plotbar.ngramf(complete.trigrams, title = "Complete Trigrams Frequency", topn = 15, gram = 3)

pnews.trigramsSW <- plotbar.ngramf(news.trigramsSW, title = "News Trigrams Frequency", topn = 15, gram = 3)
pblogs.trigramsSW <- plotbar.ngramf(blogs.trigramsSW, title = "Blogs Trigrams Frequency", topn = 15, gram = 3)
ptwitter.trigramsSW <- plotbar.ngramf(twitter.trigramsSW, title = "Twitter Trigrams Frequency", topn = 15, gram = 3)
pcomplete.trigramsSW <- plotbar.ngramf(complete.trigramsSW, title = "Complete Trigrams Frequency", topn = 15, gram = 3)

windows()
pnews.trigrams + pnews.trigramsSW
windows()
pblogs.trigrams + pblogs.trigramsSW
windows()
ptwitter.trigrams + ptwitter.trigramsSW
windows()
pcomplete.trigrams + pcomplete.trigramsSW