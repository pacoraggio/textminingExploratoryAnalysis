# name: tmEDAScriptTested
# sbj: tidyverseApproach
#      all tested calls
# author: Paolo Coraggio
# create date: 06/03/2020

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

df.summary <- read.table('./en_US/summary.txt', 
                         col.names = c("lines",
                                       "words",
                                       "characters",
                                       "bytes",
                                       "source"),
                         nrows = 3)

df.summary <- mutate(df.summary, 
                     Mb = round(bytes*0.00000095367432,2))

df.summary <- df.summary[,c(5,1,2,3,4)]
df.summary$words/df.summary$lines

## Task 1: comparing the 1% of the data versus the whole

df.news <- createSampledDfText('./en_US/en_US.news.txt', 0.01, book = "news")
df.blogs <- createSampledDfText('./en_US/en_US.blogs.txt', 0.01, book = "blogs")
df.twitter <- createSampledDfText('./en_US/en_US.twitter.txt', 0.01, book = "twitter")

save(df.news, file = "dfnews.RData")
save(df.blogs, file = "dfblogs.RData")
save(df.twitter, file = "dftwitter.RData")
save(df.complete, file = "dfcomplete.RData")

df.newsAll <- createSampledDfText('./en_US/en_US.news.txt', 1, book = "news")
df.blogsAll <- createSampledDfText('./en_US/en_US.blogs.txt', 1, book = "blogs")

## News
ncharSamp <- nchar(df.news$text)
ncharAll <- nchar(df.newsAll$text)

df.char <- data.frame("Sampled Text" = round(c(sum(ncharSamp), 
                                               mean(ncharSamp), 
                                               sd(ncharSamp), 
                                               median(ncharSamp)), 2),
                      "All Text" = round(c(sum(ncharAll), 
                                           mean(ncharAll), 
                                           sd(ncharAll), 
                                           median(ncharAll)),2),
                      row.names = c("Sum", "Mean", "Standard Deviation", "Median"))

save(df.char, file = "dataChar.RData")

## Blogs

ncharBlogsSamp <- nchar(df.blogs$text)
ncharBlogsAll <- nchar(df.blogsAll$text)

df.Blogschar <- data.frame("Sampled Text" = round(c(sum(ncharBlogsSamp), 
                                               mean(ncharBlogsSamp), 
                                               sd(ncharBlogsSamp), 
                                               median(ncharBlogsSamp)), 2),
                      "All Text" = round(c(sum(ncharBlogsAll), 
                                           mean(ncharBlogsAll), 
                                           sd(ncharBlogsAll), 
                                           median(ncharBlogsAll)),2),
                      row.names = c("Sum", "Mean", "Standard Deviation", "Median"))

save(df.Blogschar, file = "dataBlogChar.RData")
save(df.blogsAll, file = "dfblogsall.RData")


# df.ncharnews <- data.frame(nchar = ncharSamp,
#                            group = "sampled")
# df.ncharnewsAll <- data.frame(nchar = ncharAll,
#                            group = "All")
# 
# plot.data <- rbind(df.ncharnews, df.ncharnewsAll)
# 
# windows()
# ggplot(plot.data, aes(x = group, y = nchar, fill = group)) +
#     geom_boxplot()
# 

df.complete <- rbind(df.news, df.blogs, df.twitter)
df.complete$doc_id <- 1:nrow(df.complete)


## Cleaning text

load(df.news)
load("dfcomplete.RData")

df.twitter$text <- clean.text(df.twitter$text)
df.news$text <- clean.text(df.news$text)
df.blogs$text <- clean.text(df.blogs$text)
df.complete$text <- clean.text(df.complete$text)

df.blogsAll$text <- clean.text(df.blogsAll$text)

## profanity filter
library(sentimentr)

a2 <- grep("\\bshit\\b", df.blogs$text, value = TRUE)
a2[1]

library(tm)

custom.stopwords <- data.frame(word = stopwords('english'),
                               lexicon = "mylexicon")

profanity.list <- data.frame(word = lexicon::profanity_arr_bad,
                             lexicon = "profanity_arr_bad")

head(profanity.list)
head(custom.stopwords)

df.blogs$text <- removeWords(df.blogs$text, profanity.list$word)
df.blogsAll$text <- removeWords(df.blogsAll$text, profanity.list$word)

save(df.blogs, file = "tidyDfSampledBlog.RData")
save(df.blogsAll, file = "tidyDfAllBlog.RData")

## Word Frequency
remove(a1, a2, ncharBlogsAll, ncharBlogsSamp, r)

blogs.wf_sw <- word.frequency(df.corpus = df.blogs, remove.stopwords = TRUE)
blogs.wf <- word.frequency(df.corpus = df.blogs, remove.stopwords = FALSE)

tic()
blogsAll.wf_sw <- word.frequency(df.corpus = df.blogsAll, remove.stopwords = TRUE)
blogsAll.wf <- word.frequency(df.corpus = df.blogsAll, remove.stopwords = FALSE)
toc()


save(blogs.wf, file = "blogSampledWordFrequency.RData")
save(blogs.wf_sw, file = "blogSampledWordFrequencyWStopWord.RData")
save(blogsAll.wf, file = "blogAlldWordFrequency.RData")
save(blogsAll.wf_sw, file = "blogAlldWordFrequencyWStopWord.RData")

names(blogs.wf)
head(blogs.wf)

head(blogs.wf_sw)
head(blogsAll.wf_sw)


# windows()
# ggplot(blogs.wf_sw[blogs.wf_sw$frequency > 200,], aes(x = frequency)) +
#     geom_histogram(bins = 30)
# windows()
# ggplot(blogsAll.wf_sw[blogsAll.wf_sw$frequency > 20000,], aes(x = frequency)) +
#     geom_histogram(bins = 30)

p1 <- plotbar.wf(blogs.wf)
p2 <- plotbar.wf(blogs.wf_sw)

p3 <- plotbar.wf(blogsAll.wf)
p4 <- plotbar.wf(blogsAll.wf_sw)

windows()
((p1/p3) + (p2/p4))

windows()
(p1 + p2)/(p3 + p4)


library(wordcloud)
set.seed(100)
wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))

# 153.14 sec elapsed

head(blogs.wf_sw)
head(blogsAll.wf_sw)

## Optional: comparing the dataset with stopwords or without stopwords

news.wf_sw <- word.frequency(df.corpus = df.news, remove.stopwords = TRUE)
blogs.wf_sw <- word.frequency(df.corpus = df.blogs, remove.stopwords = TRUE)
twitter.wf_sw <- word.frequency(df.corpus = df.twitter, remove.stopwords = TRUE)
complete.wf_sw <- word.frequency(df.corpus = df.complete, remove.stopwords = TRUE)

news.wf <- word.frequency(df.corpus = df.news, remove.stopwords = FALSE)
blogs.wf <- word.frequency(df.corpus = df.blogs, remove.stopwords = FALSE)
twitter.wf <- word.frequency(df.corpus = df.twitter, remove.stopwords = FALSE)
complete.wf <- word.frequency(df.corpus = df.complete, remove.stopwords = FALSE)


# anti profanity complete.wf[complete.wf$word %in% lexicon::profanity_zac_anger,]
## Plotting word frequency with stopwords

p.news_sw <- plotbar.wf(news.wf_sw, "News")
p.blogs_sw <- plotbar.wf(blogs.wf_sw, "Blogs")
p.twitter_sw <- plotbar.wf(twitter.wf_sw, "Twitter")
p.complete_sw <- plotbar.wf(complete.wf_sw, "Total")

windows()
((p.news_sw + p.blogs_sw + p.twitter_sw) / p.complete_sw)

## Plotting word frequency without stopwords

p.news <- plotbar.wf(news.wf, "News")
p.blogs <- plotbar.wf(blogs.wf, "Blogs")
p.twitter <- plotbar.wf(twitter.wf, "Twitter")
p.complete <- plotbar.wf(complete.wf, "Total")

windows()
((p.news + p.blogs + p.twitter) / p.complete)

windows()
p.complete / p.complete_sw

nrow(complete.wf[complete.wf$frequency < 5,])/nrow(complete.wf) 

## Computing n-grams 2,3,4,5
source("tidyNgrams.R")
source("plotcustomf.R")

testNewsNotTidy <- create.trigramsfreq(df.news)
head(testNewsNotTidy)
testNewsTidy <- create.trigramsfreq(df.news, remove.stopwords = TRUE)
head(testNewsTidy)

testnewstetraNotTidy <- create.tetragramsfreq(df.complete)
head(testnewstetraNotTidy)
plotbar.ngramf(testnewstetraNotTidy)
remove(testnewstetraNotTidy)
testnewstetraTidy <- create.tetragramsfreq(df.news, remove.stopwords = TRUE)
head(testnewstetraTidy)
plotbar.ngramf(testnewstetraTidy)
remove(testnewstetraTidy)

pentaNotTidy <- create.pentagramfreq(df.complete)
head(pentaNotTidy)
plotbar.ngramf(pentaNotTidy)
remove(pentaNotTidy)
pentaTidy <- create.pentagramfreq(df.blogs, remove.stopwords = TRUE)
head(pentaTidy)
windows()
plotbar.ngramf(pentaTidy)
