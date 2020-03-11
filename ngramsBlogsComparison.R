# name: ngramsBlogsComparison.R
# sbj: n-grams frequency comparison
#      Sampled vs Complete text from Blogs
# type: script
# author: Paolo Coraggio
# create date: 10/03/2020


# bigrams Blogs sampled vs Blogs all

rm(list = ls())
options(stringsAsFactors = FALSE)
# Sys.setlocale('LC_ALL', 'C')

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


## loading data - without profanity

load("tidyDfAllBlog.RData")
load("tidyDfSampledBlog.RData")

## sampled
tic()
sampledBlogs.trigramsfreq <- create.trigramsfreq(df.blogs,remove.stopwords = FALSE)
sampledBlogs.trigramsfreq_sw <- create.trigramsfreq(df.blogs,remove.stopwords = TRUE)
toc()
# 6.83 sec elapsed

## all
tic()
completeBlogs.trigramsfreq <- create.trigramsfreq(df.blogsAll,remove.stopwords = FALSE)
completeBlogs.trigramsfreq_sw <- create.trigramsfreq(df.blogsAll,remove.stopwords = TRUE)
toc()
# 1414.39 sec elapsed

save(sampledBlogs.trigramsfreq, file = "sampledBlogs_trigramsfreq.RData")
save(sampledBlogs.trigramsfreq_sw, file = "sampledBlogs_trigramsfreqSW.RData")
save(completeBlogs.trigramsfreq, file = "completeBlogs_trigramsfreq.RData")
save(completeBlogs.trigramsfreq_sw, file = "completeBlogs_trigramsfreq_sw")

load("sampledBlogs_trigramsfreq.RData")
windows()
plotbar.ngramf(sampledBlogs.trigramsfreq, title = "trigrams", gram = 3)

## sampled
tic()
sampledBlogs.tetragramsfreq <- create.tetragramsfreq(df.blogs,remove.stopwords = FALSE)
sampledBlogs.tetragramsfreq_sw <- create.tetragramsfreq(df.blogs,remove.stopwords = TRUE)
toc()
# 7.06 sec elapsed

## all
tic()
completeBlogs.tetragramsfreq <- create.tetragramsfreq(df.blogsAll,remove.stopwords = FALSE)
completeBlogs.tetragramsfreq_sw <- create.tetragramsfreq(df.blogsAll,remove.stopwords = TRUE)
toc()
# # Error: cannot allocate vector of size 215.9 Mb 

