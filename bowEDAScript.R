# name: bowEDAScript.R
# sbj: bag of words Approach
# author: Paolo Coraggio
# create date: 02/03/2020

rm(list = ls())
options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL', 'C')
library(tm)
library(stringi)
library(qdap)

source("tryTolower.R")
source("createSampledText.R")
source("cleanCorpus.R")
# building the dataset

df.news <- createSampledDfText('./en_US/en_US.news.txt', 0.01)
df.blogs <- createSampledDfText('./en_US/en_US.blogs.txt', 0.01)
df.twitter <- createSampledDfText('./en_US/en_US.twitter.txt', 0.01)
df.news$text[4]
## TEXT CLEANING TASK
# Basic
#  Lowering text
#  Removing Punctuation
#  Stripping extra whitespace
#  Removing numbers 
#  Removing stopwords
#  Stemming Documents
# all performed by a tm function

custom.stopwords <- c(stopwords('english'), 'lol', 'smh', 'rt')

corpus.news <- VCorpus(DataframeSource(df.news))
corpus.blogs <- VCorpus(DataframeSource(df.blogs))
corpus.twitter <- VCorpus(DataframeSource(df.twitter))

# as.list(corpus.news)[1]
# nchar(df.news[1,]$text)

corpus.news <- clean.corpus(corpus.news)
corpus.blogs <- clean.corpus(corpus.blogs)
corpus.twitter <- clean.corpus(corpus.twitter)

# as.list(corpus.news)[[1]]
# lm <- lapply(df.news$text, which_misspelled)
## Frequent Terms and Associations

tdm.news <- TermDocumentMatrix(corpus.news, control = list(weighting = weightTf)) 
tmd.news.m <- as.matrix(tdm.news)

tdm.blogs <- TermDocumentMatrix(corpus.blogs, control = list(weighting = weightTf)) 
tmd.blogs.m <- as.matrix(tdm.blogs)

tdm.twitter <- TermDocumentMatrix(corpus.twitter, control = list(weighting = weightTf)) 
tmd.twitter.m <- as.matrix(tdm.twitter)

news.termfreq <- rowSums(tmd.news.m)
blogs.termfreq <- rowSums(tmd.blogs.m)
twitter.termfreq <- rowSums(tmd.twitter.m)

dfnews.freq <- data.frame(word = names(news.termfreq), frequency = news.termfreq)
dfnews.freq <- dfnews.freq[order(dfnews.freq[,2], decreasing = TRUE),]

dfblogs.freq <- data.frame(word = names(blogs.termfreq), frequency = blogs.termfreq)
dfblogs.freq <- dfblogs.freq[order(dfblogs.freq[,2], decreasing = TRUE),]

dftwitter.freq <- data.frame(word = names(twitter.termfreq), frequency = twitter.termfreq)
dftwitter.freq <- dftwitter.freq[order(dftwitter.freq[,2], decreasing = TRUE),]

dfnews.freq[1:10,]
dfblogs.freq[1:10,]
dftwitter.freq[1:10,]

save(tdm.news, file = "tdmnews.RData")
save(tdm.blogs, file = "tdmblogs.RData")
save(tdm.twitter, file = "tdmtwitter.RData")

save(tmd.news.m, file = "tmdnewsmat.RData")
save(tmd.blogs.m, file = "tmdblogsmat.RData")
save(tmd.twitter.m, file = "tmdtwittermat.RData")

save(dfnews.freq, file = "dfnewsfreq.RData")
save(dfblogs.freq, file = "dfblogsfreq.RData")
save(dftwitter.freq, file = "dftwitterfreq.RData")

