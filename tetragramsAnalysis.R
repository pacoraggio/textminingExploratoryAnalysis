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

# load("news_2percent.RData")
# load("blogs_2percent.RData")
# load("twitter_2percent.RData")
# load("complete_2percent.RData")

load("dfnewsNP.RData")
load("dfblogsNP.RData")
load("dftwitterNP.RData")
load("dfcompleteNP.RData")


tic()
news.tetragrams <- create.tetragramsfreq(df.news)
blogs.tetragrams <- create.tetragramsfreq(df.blogs)
twitter.tetragrams <- create.tetragramsfreq(df.twitter)
complete.tetragrams <- create.tetragramsfreq(df.complete)
toc()
# 48.8 sec elapsed - 97.89 sec elapsed


tic()
news.tetragramsSW <- create.tetragramsfreq(df.news, remove.stopwords = TRUE)
blogs.tetragramsSW <- create.tetragramsfreq(df.blogs, remove.stopwords = TRUE)
twitter.tetragramsSW <- create.tetragramsfreq(df.twitter, remove.stopwords = TRUE)
complete.tetragramsSW <- create.tetragramsfreq(df.complete, remove.stopwords = TRUE)
toc()
# 19.48 sec elapsed - 46.16 sec elapsed

pnews.tetragrams <- plotbar.ngramf(news.tetragrams, title = "News Tetragrams Frequency", topn = 15, gram = 4)
pblogs.tetragrams <- plotbar.ngramf(blogs.tetragrams, title = "Blogs Tetragrams Frequency", topn = 15, gram = 4)
ptwitter.tetragrams <- plotbar.ngramf(twitter.tetragrams, title = "Twitter Tetragrams Frequency", topn = 15, gram = 4)
pcomplete.tetragrams <- plotbar.ngramf(complete.tetragrams, title = "Complete Tetragrams Frequency", topn = 15, gram = 4)

pnews.tetragramsSW <- plotbar.ngramf(news.tetragramsSW, title = "News Tetragrams Frequency", topn = 15, gram = 4)
pblogs.tetragramsSW <- plotbar.ngramf(blogs.tetragramsSW, title = "Blogs Tetragrams Frequency", topn = 15, gram = 4)
ptwitter.tetragramsSW <- plotbar.ngramf(twitter.tetragramsSW, title = "Twitter Tetragrams Frequency", topn = 15, gram = 4)
pcomplete.tetragramsSW <- plotbar.ngramf(complete.tetragramsSW, title = "Complete Tetragrams Frequency", topn = 15, gram = 4)

windows()
pnews.tetragrams + pnews.tetragramsSW
windows()
pblogs.tetragrams + pblogs.tetragramsSW
windows()
ptwitter.tetragrams + ptwitter.tetragramsSW
windows()
pcomplete.tetragrams + pcomplete.tetragramsSW

