# name: unigramsBlogsComparison.R
# sbj: unigrams frequency comparison
#      Sampled vs Complete text from Blogs
# type: script
# author: Paolo Coraggio
# create date: 10/03/2020

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

## Creating the datasets to compare

df.blogs <- createSampledDfText('./en_US/en_US.blogs.txt', 0.01, book = "blogs")
df.blogs_complete <- createDfText('./en_US/en_US.blogs.txt', book = "complete")

dim(df.blogs)
dim(df.blogs_complete)

## number of char, mean and sd of the differet lines
ncharBlogsSamp <- nchar(df.blogs$text)
ncharBlogsAll <- nchar(df.blogs_complete$text)

df.Blogschar <- data.frame("Sampled Text" = round(c(sum(ncharBlogsSamp), 
                                                    mean(ncharBlogsSamp), 
                                                    sd(ncharBlogsSamp), 
                                                    median(ncharBlogsSamp)), 2),
                           "All Text" = round(c(sum(ncharBlogsAll), 
                                                mean(ncharBlogsAll), 
                                                sd(ncharBlogsAll), 
                                                median(ncharBlogsAll)),2),
                           row.names = c("Sum", "Mean", "Standard Deviation", "Median"))

df.Blogschar
save(df.Blogschar, file = "dataBlogChar.RData")

## Cleaning Text

tic()
df.blogs$text <- clean.text(df.blogs$text)
df.blogs_complete$text <- clean.text(df.blogs_complete$text)
toc()
# 84.35 sec elapsed - 89.3 sec elapsed - 89.06 sec elapsed

## Profanity Filter

library(sentimentr)

profanity.list <- grep("^[a-z]",tolower(lexicon::profanity_alvarez), value = TRUE)
profanity.list <- grep("[a-z]$",profanity.list, value = TRUE)
profanity.list <- c(profanity.list, "bullshit", "bullshits", "motherfuckers")
profanity.df <- data.frame(word = profanity.list,
                           lexicon = "custom profanity list")

tic()
df.blogs$text <- removeWords(df.blogs$text, profanity.df$word)
df.blogs_complete$text <- removeWords(df.blogs_complete$text, profanity.df$word)
toc()

# 121.4 sec elapsed
(df.Blogschar$All.Text[1] - sum(nchar(df.blogs_complete$text)))/sum(nchar(df.blogs_complete$text))
# 0.0005032061 - 0.04146954 - 0.04146954

# saving the cleaned text 
save(df.blogs, file = "dfblogs1percentcleaned.RData")
save(df.blogs_complete, file = "dfblogscompletecleaned.RData")

## Word frequency

tic()
blogs.wf_sw <- word.frequency(df.corpus = df.blogs, remove.stopwords = TRUE)
blogs.wf <- word.frequency(df.corpus = df.blogs, remove.stopwords = FALSE)
toc()
# 1.56 sec elapsed - 1.86 sec elapsed - 1.48 sec elapsed
# 2.9 sec elapsed for 2 percent
tic()
blogscomplete.wf_sw <- word.frequency(df.corpus = df.blogs_complete, remove.stopwords = TRUE)
blogscompleteAll.wf <- word.frequency(df.corpus = df.blogs_complete, remove.stopwords = FALSE)
toc()
# 149.82 sec elapsed - 148.51 sec elapsed - 144.64 sec elapsed
head(blogs.wf)
head(blogscompleteAll.wf)

head(blogscomplete.wf_sw)
head(blogs.wf_sw)

# saving word frequency
save(blogs.wf, file = "blogs1percentWF.RData")
save(blogs.wf_sw, file = "blogs1percentWFSW.RData")
save(blogscompleteAll.wf, file = "blogscompleteWF.RData")
save(blogscomplete.wf_sw, file = "blogscompleteWFSW.RData")

tic()
plot.sampled <- plotbar.wf(blogs.wf, title = "Sampled", topn = 20)
plot.sampledSW <- plotbar.wf(blogs.wf_sw, title = "Sampled with SW", topn = 20)
plot.complete <- plotbar.wf(blogscompleteAll.wf, title = "Completed", topn = 20)
plot.completeSW <- plotbar.wf(blogscomplete.wf_sw, title = "Completed with SW", topn = 20)
toc()
# 0.72 sec elapsed - 1.02 sec elapsed

save(plot.sampled, file = "plot_unisample.RData")
save(plot.sampledSW, file = "plot_unisampleSW.RData")
save(plot.complete, file = "plot_unicomplete.RData")
save(plot.completeSW, file = "plot_unicompleteSW.RData")

library(patchwork)
source("plotcustomf.R")

windows()
# plot.sampled <- plotbar.wf(blogs.wf, title = "Sampled", topn = 15)

patchwork <- ((plot.sampled + plot.complete) / (plot.sampledSW + plot.completeSW))
patchwork
windows()
patchwork + plot_annotation(
    title = 'Unigrams Comparison between Sampled and Complete Blogs dataset',
    subtitle = 'Comparison without considering stop words (top) and with (bottom)',
    caption = 'The comparison shows a good approximation with just 1% of the sampled dataset'
)


### 
load("blogs1percentWF.RData")
load("blogs1percentWFSW.RData")
load("blogscompleteWF.RData")
load("blogscompleteWFSW.RData")


length(unique(blogs.wf$word))
length(unique(blogscompleteAll.wf$word))

nrow(blogs.wf[blogs.wf$frequency == 1,])/nrow(blogs.wf)
nrow(blogscompleteAll.wf[blogscompleteAll.wf$frequency == 1,])/nrow(blogscompleteAll.wf)

psampledwf <- c()
pallwf <- c()
for(i in 2:150)
{
    psampledwf <- c(psampledwf, nrow(blogs.wf[blogs.wf$frequency < i,])/nrow(blogs.wf))
    pallwf <- c(pallwf, nrow(blogscompleteAll.wf[blogscompleteAll.wf$frequency < i,])/nrow(blogscompleteAll.wf))
}

df.freqpercentSampled <- data.frame(frequency = 2:150,
                                    percentage = psampledwf,
                                    type = rep("Sampled", length(2:150)))

df.freqpercentAll <- data.frame(frequency = 2:150,
                                    percentage = pallwf,
                                    type = rep("All", length(2:150)))

df.wf <- rbind(df.freqpercentSampled, df.freqpercentAll)

head(df.wf)

min_95_samp <- df.wf %>%
    filter(percentage > 0.95 & type == "Sampled") %>%
    head(n = 1)

min_95_All <- df.wf %>%
    filter(percentage > 0.95 & type == "All") %>%
    head(n = 1)


p.freq <- ggplot(df.wf, aes(frequency, percentage, color = type)) +
    geom_point() +
    scale_color_manual(values = c("#00AFBB", "#FC4E07")) +
    geom_hline(yintercept = 0.95, color = "black", size = 1.25, linetype="dashed") +
    geom_text(aes(x=0.0,label = "0.95", y=0.97), 
              colour="black", angle=0) + 
    geom_vline(xintercept = min_95_samp$frequency, 
               color = "#FC4E07") +
    geom_text(aes(x=min_95_samp$frequency, label=min_95_samp$frequency, y=0.0), 
              colour="#FC4E07", angle=0) +
    geom_vline(xintercept = min_95_All$frequency, color = "#00AFBB") +
    geom_text(aes(x=min_95_All$frequency, label=min_95_All$frequency, y=0.0), 
              colour="#00AFBB", angle=0) +
    xlab("word frequency") + ylab("unique words percentage") +        
    theme_solarized_2(base_size = 12) +
    ggtitle("Unique words percentage")

save(p.freq, file = "plotfreqBlogs.RData")
### 



frequencySampled <- sort(unique(blogs.wf$frequency))
frequencyAll <- sort(unique(blogscompleteAll.wf$frequency))

max.lengthSampled <- c()
unique.wordsSampled <- c()
sum.frequencySampled <- c()

for(i in frequencySampled){
    max.lengthSampled <- c(max.lengthSampled, max(nchar(blogs.wf[blogs.wf$frequency == i,]$word)))
    unique.wordsSampled <- c(unique.wordsSampled, length(unique(blogs.wf[blogs.wf$frequency == i,]$word)))
    sum.frequencySampled <- c(sum.frequencySampled, sum(blogs.wf[blogs.wf$frequency == i,]$frequency))
}

max.lengthAll <- c()
unique.wordsAll <- c()
sum.frequencyAll <- c()

for(i in frequencyAll){
    max.lengthAll <- c(max.lengthAll, max(nchar(blogscompleteAll.wf[blogscompleteAll.wf$frequency == i,]$word)))
    unique.wordsAll <- c(unique.wordsAll, length(unique(blogscompleteAll.wf[blogscompleteAll.wf$frequency == i,]$word)))
    sum.frequencyAll <- c(sum.frequencyAll, sum(blogscompleteAll.wf[blogscompleteAll.wf$frequency == i,]$frequency))
}

df.dataSampled <- data.frame(frequency = 15:180,
                            word.maxlength = unique.wordsSampled[15:180],
                            word.sumfrequency =  sum.frequencySampled[15:180],
                            type = rep('Sampled', 166))

df.dataAll <- data.frame(frequency = 15:180,
                             word.maxlength = unique.wordsAll[15:180],
                             word.sumfrequency =  sum.frequencyAll[15:180],
                             type = rep('All', 166))

# length(15:180)

df.data <- rbind(df.dataSampled, df.dataAll)

windows()
ggplot(df.data, aes(frequency,word.maxlength)) +
    geom_point() +
    geom_hline(yintercept = 18, color = "red") +
    facet_grid(~type) 
    
names(blogs.wf)
a1 <- sort(blogs.wf[nchar(blogs.wf$word) == 18,]$word)
a2 <- sort(blogscompleteAll.wf[nchar(blogscompleteAll.wf$word) == 18,]$word)

# 75% of words

min_75_samp <- df.wf %>%
    filter(percentage > 0.75 & type == "Sampled") %>%
    head(n = 1)

min_75_All <- df.wf %>%
    filter(percentage > 0.75 & type == "All") %>%
    head(n = 1)

min_80_All
min_80_samp

wf.blogsSampled80 <- blogs.wf[blogs.wf$frequency < 6,]
wf.blogsAll80 <- blogscompleteAll.wf[blogscompleteAll.wf$frequency < 6,]

dim(wf.blogsAll80)
dim(wf.blogsSampled80)

mean(nchar(wf.blogsSampled80$word))
mean(nchar(wf.blogsAll80$word))

