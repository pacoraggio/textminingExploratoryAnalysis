# name: bigramsBlogsComparison.R
# sbj: bigrams frequency comparison
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
sampledBlogs.bigramfreq <- create.bigramsfreq(df.blogs,remove.stopwords = FALSE)
sampledBlogs.bigramfreq_sw <- create.bigramsfreq(df.blogs,remove.stopwords = TRUE)
toc()
# 3.53 sec elapsed

## all
tic()
completeBlogs.bigramfreq <- create.bigramsfreq(df.blogsAll,remove.stopwords = FALSE)
completeBlogs.bigramfreq_sw <- create.bigramsfreq(df.blogsAll,remove.stopwords = TRUE)
toc()
# 312.28 sec elapsed


plot.sampledbigrams <- plotbar.ngramf(sampledBlogs.bigramfreq, "Sampled")
plot.sampledbigramsSW <-plotbar.ngramf(sampledBlogs.bigramfreq_sw, "Sampled with SW")

plot.allbigrams <- plotbar.ngramf(completeBlogs.bigramfreq, "Complete")
plot.allbigramsSW <- plotbar.ngramf(completeBlogs.bigramfreq_sw, "Complete with SW")

windows()
(plot.sampledbigrams + plot.allbigrams) / (plot.sampledbigramsSW + plot.allbigramsSW)

source("plotcustomf.R")
plot.sampledbigrams <- plotbar.ngramf(sampledBlogs.bigramfreq, title = "Sampled Blogs Bigrams Frequency")

patchwork <- (plot.sampledbigrams + plot.sampledbigramsSW) / (plot.allbigrams + plot.allbigramsSW)
windows()
patchwork + plot_annotation(
    title = 'The surprising truth about mtcars',
    subtitle = 'These 3 plots will reveal yet-untold secrets about our beloved data-set',
    caption = 'Disclaimer: None of these plots are insightful'
)


save(sampledBlogs.bigramfreq, file = "sampledBlogs_bigramfreq.RData")
save(sampledBlogs.bigramfreq_sw, file = "sampledBlogs_bigramfreqSW.RData")
save(completeBlogs.bigramfreq, file = "completeBlogs_bigramfreq.RData")
save(completeBlogs.bigramfreq_sw, file = "completeBlogs_bigramfreqSW.RData")


load("sampledBlogs_bigramfreq.RData")
load("sampledBlogs_bigramfreqSW.RData")
load("completeBlogs_bigramfreq.RData")
load("completeBlogs_bigramfreqSW.RData")

source("plotcustomf.R")
windows()
plot.sampledbigrams <- plotbar.ngramf(sampledBlogs.bigramfreq, "Sampled", topn = 10, gram = 2)
plot.sampledbigramsSW <-plotbar.ngramf(sampledBlogs.bigramfreq_sw, "Sampled with SW", topn = 10, gram = 2)
plot.allbigrams <- plotbar.ngramf(completeBlogs.bigramfreq, "Complete", topn = 10, gram = 2)
plot.allbigramsSW <- plotbar.ngramf(completeBlogs.bigramfreq_sw, "Complete with SW", topn = 10, gram = 2)

save(plot.sampledbigrams, file = "plot_sampledbigrams.RData")
save(plot.sampledbigramsSW, file = "plot_sampledbigramsSW.RData")
save(plot.allbigrams, file = "plot_allbigrams.RData")
save(plot.allbigramsSW, file = "plot_allbigramsSW.RData")

