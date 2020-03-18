# name: wordfrequencyAnalysis.R
# sbj: word frequency analysis
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

load("news_2percentWP.RData")
load("blogs_2percentWP.RData")
load("twitter_2percentWP.RData")
load("complete_2percentWP.RData")

# Word frequency with profanity
tic()
df.news_wfP <- word.frequency(df.news_withprofanity)
df.blogs_wfP <- word.frequency(df.blogs_withprofanity)
df.twitter_wfP <- word.frequency(df.twitter_withprofanity)
df.complete_wfP <- word.frequency(df.complete_withprofanity)
toc()
# 9.13 sec elapsed - 7.51 sec elapsed

save(df.news_wfP, file = "dfnews_wfProf.RData")
save(df.blogs_wfP, file = "dfblogs_wfProf.RData")
save(df.twitter_wfP, file = "dftwitter_wfProf.RData")
save(df.complete_wfP, file = "dfcomplete_wfProf.RData")

# Word frequency without profanity
tic()
df.news_wf <- word.frequency(df.news)
df.blogs_wf <- word.frequency(df.blogs)
df.twitter_wf <- word.frequency(df.twitter)
df.complete_wf <- word.frequency(df.complete)
toc()
# 7.41 sec elapsed - 7.5 sec elapsed

save(df.news_wf, file = "dfnews_wf.RData")
save(df.blogs_wf, file = "dfblogs_wf.RData")
save(df.twitter_wf, file = "dftwitter_wf.RData")
save(df.complete_wf, file = "dfcomplete_wf.RData")

tic()
df.news_wfSW <- word.frequency(df.news, remove.stopwords = TRUE)
df.blogs_wfSW <- word.frequency(df.blogs, remove.stopwords = TRUE)
df.twitter_wfSW <- word.frequency(df.twitter, remove.stopwords = TRUE)
df.complete_wfSW <- word.frequency(df.complete, remove.stopwords = TRUE)
toc()

save(df.news_wfSW, file = "dfnews_wfSW.RData")
save(df.blogs_wfSW, file = "dfblogs_wfSW.RData")
save(df.twitter_wfSW, file = "dftwitter_wfSW.RData")
save(df.complete_wfSW, file = "dfcomplete_wfSW.RData")

## mean character lenght for each dataset (whithout stop words)
length(unique(df.news_wf$word))

df.charstat <- data.frame("total number" = c(sum(df.news_wf$frequency),
                                             sum(df.blogs_wf$frequency),
                                             sum(df.twitter_wf$frequency),
                                             sum(df.complete_wf$frequency)),
                          "unique words" = c(length(unique(df.news_wf$word)),
                                             length(unique(df.blogs_wf$word)),
                                             length(unique(df.twitter_wf$word)),
                                             length(unique(df.complete_wf$word))),
                          "max length" = c(max(nchar(df.news_wf$word)),
                                           max(nchar(df.blogs_wf$word)),
                                           max(nchar(df.twitter_wf$word)),
                                           max(nchar(df.complete_wf$word))),
                          "mean" = c(mean(nchar(df.news_wf$word)),
                                     mean(nchar(df.blogs_wf$word)),
                                     mean(nchar(df.twitter_wf$word)),
                                     mean(nchar(df.complete_wf$word))),
                          "sd" = c(sd(nchar(df.news_wf$word)),
                                   sd(nchar(df.blogs_wf$word)),
                                   sd(nchar(df.twitter_wf$word)),
                                   sd(nchar(df.complete_wf$word))),
                          row.names = c("news", "blogs", "twitter", "complete")
)
save(df.charstat, file = "wordstat.RData")

## mean character lenght for each dataset (with stop words)
df.charstatSW <- data.frame("total number" = c(sum(df.news_wfSW$frequency),
                                               sum(df.blogs_wfSW$frequency),
                                               sum(df.twitter_wfSW$frequency),
                                               sum(df.complete_wfSW$frequency)),
                            "unique words" = c(length(unique(df.news_wfSW$word)),
                                               length(unique(df.blogs_wfSW$word)),
                                               length(unique(df.twitter_wfSW$word)),
                                               length(unique(df.complete_wfSW$word))),
                            "max length" = c(max(nchar(df.news_wfSW$word)),
                                             max(nchar(df.blogs_wfSW$word)),
                                             max(nchar(df.twitter_wfSW$word)),
                                             max(nchar(df.complete_wfSW$word))),
                            "mean" = c(mean(nchar(df.news_wfSW$word)),
                                       mean(nchar(df.blogs_wfSW$word)),
                                       mean(nchar(df.twitter_wfSW$word)),
                                       mean(nchar(df.complete_wfSW$word))),
                            "sd" = c(sd(nchar(df.news_wfSW$word)),
                                     sd(nchar(df.blogs_wfSW$word)),
                                     sd(nchar(df.twitter_wfSW$word)),
                                     sd(nchar(df.complete_wfSW$word))),
                            row.names = c("news", "blogs", "twitter", "complete")
)
save(df.charstatSW, file = "wordstatSW.RData")

df.blogslongestwords <- df.blogs_wf[nchar(df.blogs_wf$word) > 20,]
df.newslongestwords <- df.news_wf[nchar(df.news_wf$word) > 20,]
df.twitterlongestwords <- df.twitter_wf[nchar(df.twitter_wf$word)>20,]


df.blogslongestwords <- df.blogslongestwords[with(df.blogslongestwords,order(nchar(df.blogslongestwords$word),
                                                     decreasing = TRUE)),]
df.newslongestwords <- df.newslongestwords[with(df.newslongestwords,order(nchar(df.newslongestwords$word),
                                                     decreasing = TRUE)),]
df.twitterlongestwords <- df.twitterlongestwords[with(df.twitterlongestwords,order(nchar(df.twitterlongestwords$word),
                                                     decreasing = TRUE)),]

save(df.newslongestwords, file = "newslongest.RData")
save(df.blogslongestwords, file = "blogslongest.RData")
save(df.twitterlongestwords, file = "twitterlongest.Rdata")

head(df.newslongestwords)
head(df.blogslongestwords)
head(df.twitterlongestwords)

tail(df.blogslongestwords)
tail(df.newslongestwords)
tail(df.twitterlongestwords)

df.blogs_wf[nchar(df.blogs_wf$word) == max(nchar(df.blogs_wf$word)),]$word
df.twitter.wf[nchar(twitter.wf$word) == max(nchar(twitter.wf$word)),]$word
news.wf[nchar(news.wf$word) == max(nchar(news.wf$word)),]$word
# "wwwdnrstatemiusspatialdatalibrarypdfmapsmineralleaseinformationoaklandnominationspdf"

wwwcomplete <- grep("^www", df.complete$text, value = TRUE)
wwwnews <- grep("^www", df.news$text, value = TRUE)

head(news.wf)
# 8 sec elapsed - 7.92 sec elapsed

r2 <- grep("shit", complete.wfSW$word, value = TRUE)
length(r2)

# 42 occurrences without tyding

tail(complete.wf$frequency)
summary(complete.wf$frequency)
nrow(complete.wf)
nrow(complete.wf[complete.wf$frequency == 1,])

nrow(complete.wf[complete.wf$frequency < 10,])/nrow(complete.wf)
sum(news.wfSW$frequency)

source("plotcustomf.R")
windows()
pnews <- plotbar.wf(df.news_wf, title = "News Word Frequency", topn = 15)
pblogs <- plotbar.ngramf(df.blogs_wf, title = "Blogs Word Frequency", topn = 15)
ptwitter <- plotbar.ngramf(df.twitter_wf, title = "Twitter Word Frequency", topn = 15)
pcomplete <- plotbar.ngramf(df.complete_wf, title = "Complete Word Frequency", topn = 15)

pnewsSW <- plotbar.ngramf(df.news_wfSW, title = "News Word Frequency (SW)", topn = 15)
pblogsSW <- plotbar.ngramf(df.blogs_wfSW, title = "Blogs Word Frequency (SW)", topn = 15)
ptwitterSW <- plotbar.ngramf(df.twitter_wfSW, title = "Twitter Word Frequency (SW)" , topn = 15)
pcompleteSW <- plotbar.ngramf(df.complete_wfSW, title = "Complete Word Frequency (SW)", topn = 15)

save(pnews, file = "plotuninews.RData")
save(pblogs, file = "plotuniblogs.RData")
save(ptwitter, file = "plotunitwitter.RData")
save(pcomplete, file = "plotunicomplete.Rdata")

save(pnewsSW, file = "plotuninewsSW.RData")
save(pblogsSW, file = "plotuniblogsSW.RData")
save(ptwitterSW, file = "plotunitwitterSW.RData")
save(pcompleteSW, file = "plotunicompleteSW.Rdata")


#     , "plotuniblogs.RData", "plotunitwitter.RData", "plotunicomplete.RData"))

windows()
pnews + pnewsSW
windows()
pcomplete + pcompleteSW


news.wf[news.wf$word == "sex",]$frequency +
twitter.wf[twitter.wf$word == "sex",]$frequency +
blogs.wf[blogs.wf$word == "sex",]$frequency ==
complete.wf[complete.wf$word == "sex",]$frequency


# ## after profanity filter - to review
# 
# load("dfnewsNP.RData")
# load("dfblogsNP.RData")
# load("dftwitterNP.RData")
# load("dfcompleteNP.RData")

rm(list = ls())
load("dfnews_wfSW.RData")
load("dfblogs_wfSW.RData")
load("dftwitter_wfSW.RData")
load("dfcomplete_wfSW.RData")

load("dfnews_wf.RData")
load("dfblogs_wf.RData")
load("dftwitter_wf.RData")
load("dfcomplete_wf.RData")

head(df.blogs_wf, n = 15)
max(df.complete_wf$frequency)
max(nchar(df.complete_wf[df.complete_wf$frequency == 1,]$word))


frequency <- sort(unique(df.complete_wf$frequency))
max.length <- c()
unique.words <- c()
sum.frequency <- c()
for(i in frequency){
    max.length <- c(max.length, max(nchar(df.complete_wf[df.complete_wf$frequency == i,]$word)))
    unique.words <- c(unique.words, length(unique(df.complete_wf[df.complete_wf$frequency == i,]$word)))
    sum.frequency <- c(sum.frequency, sum(df.complete_wf[df.complete_wf$frequency == i,]$frequency))
}

df.wordlength <- data.frame(frequency = frequency,
                            max.length = max.length,
                            unique.words = unique.words,
                            sum.frequency = sum.frequency)


dim(df.wordlength)
head(df.wordlength)
windows()
ggplot(df.wordlength[2:200,], aes(x = frequency, y = max.length)) +
    theme_solarized_2(base_size = 12) +
    xlab("Word Frequency") +
    geom_smooth() +
    ylab("Word Maximum Length") +
    geom_point()


df.wordlength[df.wordlength$unique.words > 1,]
## 255 is the largest index containing more than 1 word with that frequency

windows()
ggplot(df.wordlength[10:255,], aes(x = frequency, y = unique.words)) +
    theme_solarized_2(base_size = 12) +
    xlab("Word Frequency") +
    geom_smooth() +
    ylab("Number of unique words") +
    geom_point()

windows()
ggplot(df.wordlength[688:nrow(df.wordlength),], aes(x = frequency, y = unique.words)) +
    geom_point()

