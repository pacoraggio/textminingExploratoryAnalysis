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
rm(list = ls())

load("news_bigrams.RData")
load("blogs_bigrams.RData")
load("twitter_bigrams.RData")
load("complete_bigrams.RData")

load("complete_bigramsSW.RData")
load("news_bigramsSW.RData")
load("blogs_bigramsSW.RData")
load("twitter_bigramsSW.RData")


head(complete.bigrams[complete.bigrams$n == 1,])
head(twitter.bigrams[twitter.bigrams$n == 1,])
head(twitter.bigramsSW[twitter.bigramsSW$n == 1,])

head(news.bigrams, n = 11)
nrow(news.bigrams[news.bigrams$n == 1,])/nrow(news.bigrams)
nrow(blogs.bigrams[blogs.bigrams$n == 1,])/nrow(blogs.bigrams)
nrow(twitter.bigrams[twitter.bigrams$n == 1,])/nrow(twitter.bigrams)
nrow(complete.bigrams[complete.bigrams$n == 1,])/nrow(complete.bigrams)

nrow(news.bigrams[news.bigrams$n <= 2,])/nrow(news.bigrams)
nrow(blogs.bigrams[blogs.bigrams$n <= 2,])/nrow(blogs.bigrams)
nrow(twitter.bigrams[twitter.bigrams$n <= 2,])/nrow(twitter.bigrams)
nrow(complete.bigrams[complete.bigrams$n <= 2,])/nrow(complete.bigrams)

nrow(news.bigrams[news.bigrams$n <= 3,])/nrow(news.bigrams)
nrow(blogs.bigrams[blogs.bigrams$n <= 3,])/nrow(blogs.bigrams)
nrow(twitter.bigrams[twitter.bigrams$n <= 3,])/nrow(twitter.bigrams)
nrow(complete.bigrams[complete.bigrams$n <= 3,])/nrow(complete.bigrams)

nrow(news.bigrams[news.bigrams$n <= 4,])/nrow(news.bigrams)
nrow(blogs.bigrams[blogs.bigrams$n <= 4,])/nrow(blogs.bigrams)
nrow(twitter.bigrams[twitter.bigrams$n <= 4,])/nrow(twitter.bigrams)
nrow(complete.bigrams[complete.bigrams$n <= 4,])/nrow(complete.bigrams)

news.percentbigrams <- c()
blogs.percentbigrams <- c()
twitter.percentbigrams <- c()
complete.percentbigrams <- c()

k1 <- 1:30

for(i in k1)
{
    news.percentbigrams <- c(news.percentbigrams,
                             nrow(news.bigrams[news.bigrams$n <= i,])/nrow(news.bigrams))
    blogs.percentbigrams <- c(blogs.percentbigrams,
                             nrow(blogs.bigrams[blogs.bigrams$n <= i,])/nrow(blogs.bigrams))
    twitter.percentbigrams <- c(twitter.percentbigrams,
                             nrow(twitter.bigrams[twitter.bigrams$n <= i,])/nrow(twitter.bigrams))
    complete.percentbigrams <- c(complete.percentbigrams,
                             nrow(complete.bigrams[complete.bigrams$n <= i,])/nrow(complete.bigrams))
}

length(news.percentbigrams)

df.newsfreqpercentbi <- data.frame(frequency = k1,
                                    percentage = news.percentbigrams,
                                    type = rep("news 2-grams", length(k1)))
df.blogsfreqpercentbi <- data.frame(frequency = k1,
                                 percentage = blogs.percentbigrams,
                                 type = rep("blogs 2-grams", length(k1)))
df.twitterfreqpercentbi <- data.frame(frequency = k1,
                                 percentage = twitter.percentbigrams,
                                 type = rep("twitter 2-grams", length(k1)))
df.completefreqpercentbi <- data.frame(frequency = k1,
                                 percentage = complete.percentbigrams,
                                 type = rep("complete 2-grams", length(k1)))

df.freqpercentbi <-rbind(df.newsfreqpercentbi,
                       df.blogsfreqpercentbi,
                       df.twitterfreqpercentbi,
                       df.completefreqpercentbi)

# library(ggplot2)
unique(df.freqpercentbi$type)
df.freqpercentbi$type = factor(df.freqpercentbi$type, levels = c("news 2-grams",
                                                               "blogs 2-grams",
                                                               "twitter 2-grams", 
                                                               "complete 2-grams"))

windows()
p.freqpercentbi <- ggplot(df.freqpercentbi, aes(frequency, percentage, color = type)) +
    geom_point(size = 0.8) + 
    theme_solarized_2(base_size = 12) 

save(df.freqpercentbi, file = "df_freqpercentbi.RData")
save(p.freqpercentbi, file = "plot_freqpercentbi.RData")
    # facet_wrap(~type)
    # facet_grid(~type)

windows()
ggplot(df.completefreqpercentbi, aes(x = frequency, y = percentage)) +
    geom_point() + 
    facet_grid()
    

## bigrams max length
names(complete.bigrams)
max(nchar(complete.bigrams$bigram))
nrow(complete.bigrams)

nrow(complete.bigrams[nchar(complete.bigrams$bigram) > 40,])
a1 <- complete.bigrams[nchar(complete.bigrams$bigram) > 40,]
a1

