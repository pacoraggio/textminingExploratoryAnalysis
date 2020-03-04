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

## Common text mining visualization
### Term Frequency - Associations and Word Networks

# Sometimes merely looking at frequent terms can be interesting and 
# insightful endeavor. 
# The simple visualization from term frequency as a means to reinforce or
# deny what are my expectation.
# Text mining's association is similar to the statistical concept of 
# *correlation*. As the frequncy of a single word occurs, how correlated
# or associated is it with another word? The exploration of the term
# association can yield interesting relationships among a large set of terms.
# A word network differs from word association. A word network explores multiple
# word linkages simultaneously. The word network will have scores for pairs
# In contrast, word association scores represent the relationships of a single
# word to others, such as "text" to "mining" and "text" to "book" while 
# with word network we also have the score between "mining" and "book".

# The most common and immediate term frequency visualization is a barplot

library(ggplot2)
library(ggthemes)
library(patchwork)

load("dfnewsfreq.RData")
load("dfblogsfreq.RData")
load("dftwitterfreq.RData")


dfnews.freq$word <- factor(dfnews.freq$word, 
                           levels = unique(as.character(dfnews.freq$word)))
dfblogs.freq$word <- factor(dfblogs.freq$word, 
                            levels = unique(as.character(dfblogs.freq$word)))
dftwitter.freq$word <- factor(dftwitter.freq$word, 
                              levels = unique(as.character(dftwitter.freq$word)))

p1 <- ggplot(dfblogs.freq[1:10,], aes(x = word, y = frequency)) +
    geom_bar(stat = "identity", fill = "darkred") +
    coord_flip() +
    theme_gdocs() +
    geom_text(aes(label = frequency), 
              color = "white", hjust = 1.25, size = 5.0) +
    ggtitle("Blogs Word Frequency")

p2 <- ggplot(dfnews.freq[1:10,], aes(x = word, y = frequency)) +
    geom_bar(stat = "identity", fill = "darkred") +
    coord_flip() +
    theme_gdocs() +
    geom_text(aes(label = frequency), 
              color = "white", hjust = 1.25, size = 5.0) +
    ggtitle("News Word Frequency")

windows()
p2
p3 <- ggplot(dftwitter.freq[1:10,], aes(x = word, y = frequency)) +
    geom_bar(stat = "identity", fill = "darkred") +
    coord_flip() +
    theme_gdocs() +
    geom_text(aes(label = frequency), 
              color = "white", hjust = 1.25, size = 5.0) + 
    ggtitle("Twitter Word Frequency")

windows()
p1 + p2 + p3

## Word Association

# In text mining association is similar to correlation, that is when x appears
# the other term y is associated with it. It is not directly related to frequency
# but instead repers to the term pairing. Unlike statistical correlation, the
# range is between 0,1

load("tdmnews.RData")
load("tdmblogs.RData")
load("tdmtwitter.RData")

commonMFWords <- Reduce(intersect, list(dfblogs.freq$word[1:10],
                       dfnews.freq$word[1:10],
                       dftwitter.freq$word[1:10]))

news.willassociation <- as.data.frame(findAssocs(tdm.news, 'will', 0.08))
news.willassociation$terms <- row.names(news.willassociation)
news.willassociation$terms <- factor(news.willassociation$terms, 
                                     levels = news.willassociation$terms)

windows()
ggplot(news.willassociation, aes(y = terms)) +
    geom_point(aes(x = will), data = news.willassociation, size = 5) +
    theme_gdocs() + 
    geom_text(aes(x=will, label = will),
              color = "darkred", 
              hjust = -.25, size = 8) +
    theme(text = element_text(size = 20), axis.title.y = element_blank())

news.canassociation <- as.data.frame(findAssocs(tdm.news, 'can', 0.11))
news.canassociation$terms <- row.names(news.canassociation)
news.canassociation$terms <- factor(news.canassociation$terms, 
                                     levels = news.canassociation$terms)

windows()
ggplot(news.canassociation, aes(y = terms)) +
    geom_point(aes(x = can), data = news.canassociation, size = 5) +
    theme_gdocs() + 
    geom_text(aes(x=can, label = can),
              color = "darkred", 
              hjust = -.25, size = 8) +
    theme(text = element_text(size = 20), axis.title.y = element_blank())


twitter.willassociation <- as.data.frame(findAssocs(tdm.twitter, 'will', 0.07))
twitter.willassociation$terms <- row.names(twitter.willassociation)
twitter.willassociation$terms <- factor(twitter.willassociation$terms, 
                                     levels = twitter.willassociation$terms)

windows()
ggplot(twitter.willassociation, aes(y = terms)) +
    geom_point(aes(x = will), data = twitter.willassociation, size = 5) +
    theme_gdocs() + 
    geom_text(aes(x=will, label = will),
              color = "darkred", 
              hjust = -.25, size = 8) +
    theme(text = element_text(size = 20), axis.title.y = element_blank())
