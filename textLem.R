# name: textLem.R
# sbj: lemmatization
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

## Task 1: comparing the 1% of the data versus the whole

df.news <- createSampledDfText('./en_US/en_US.news.txt', 0.02, book = "news")
df.blogs <- createSampledDfText('./en_US/en_US.blogs.txt', 0.02, book = "blogs")
df.twitter <- createSampledDfText('./en_US/en_US.twitter.txt', 0.02, book = "twitter")

df.complete <- rbind(df.news, df.blogs, df.twitter)
df.complete$doc_id <- 1:nrow(df.complete)

tic()
df.twitter$text <- clean.text(df.twitter$text)
df.news$text <- clean.text(df.news$text)
df.blogs$text <- clean.text(df.blogs$text)
df.complete$text <- clean.text(df.complete$text)
toc()

save(df.news, file = "news_2percent.RData")
save(df.blogs, file = "blogs_2percent.RData")
save(df.twitter, file = "twitter_2percent.RData")
save(df.complete, file = "complete_2percent.RData")

# 7.43 sec elapsed

df.complete[1,]$text

# unigrams on df.complete
# blogs.wf_sw <- word.frequency(df.corpus = df.blogs, remove.stopwords = TRUE)

tic()
complete.wf <- word.frequency(df.complete,remove.stopwords = FALSE)
toc()
# 4.4 sec elapsed

tic()
complete.wfSW <- word.frequency(df.complete,remove.stopwords = TRUE)
toc()
# 4.15 sec elapsed

head(complete.wf) 
head(complete.wfSW)

library(SnowballC)

system.time(
    lemma_unique<-complete.wfSW %>%
        mutate(word_stem = wordStem(word, language="english"))
)

head(lemma_unique)
k <- lemma_unique[lemma_unique$word != lemma_unique$word_stem,]
head(k, n = 20)
