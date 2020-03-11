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

# save(blogs.wf, file = "blogSampledWordFrequency.RData")
# save(blogs.wf_sw, file = "blogSampledWordFrequencyWStopWord.RData")
# save(blogsAll.wf, file = "blogAlldWordFrequency.RData")
# save(blogsAll.wf_sw, file = "blogAlldWordFrequencyWStopWord.RData")

load("blogSampledWordFrequency.RData")
load("blogSampledWordFrequencyWStopWord.RData")
load("blogAlldWordFrequency.RData")
load("blogAlldWordFrequencyWStopWord.RData")


plot.sampled <- plotbar.wf(blogs.wf, title = "Sampled", topn = 15)
plot.sampledSW <- plotbar.wf(blogs.wf_sw, title = "Sampled with SW", topn = 15)
plot.complete <- plotbar.wf(blogsAll.wf, title = "Completed", topn = 15)
plot.completeSW <- plotbar.wf(blogsAll.wf_sw, title = "Completed with SW", topn = 15)

save(plot.sampled, file = "plot_unisample.RData")
save(plot.sampledSW, file = "plot_unisampleSW.RData")
save(plot.complete, file = "plot_unicomplete.RData")
save(plot.completeSW, file = "plot_unicompleteSW.RData")

library(patchwork)
source("plotcustomf.R")

windows()
plot.sampled <- plotbar.wf(blogs.wf, title = "Sampled", topn = 15)

patchwork <- ((plot.sampled + plot.complete) / (plot.sampledSW + plot.completeSW))


windows()
patchwork + plot_annotation(
    title = 'Unigrams Comparison between Sampled and Complete Blogs dataset',
    subtitle = 'Comparison without considering stop words (top) and with (bottom)',
    caption = 'The comparison shows a good approximation with just 1% of the sampled dataset'
)
# top20.wfSampledBlogs <- head(blogs.wf, n = 20)
# top20.wfSampledBlogs$type <- "Sampled Blogs"
# top20.wfSampledBlogsSW <- head(blogs.wf_sw, n = 20)
# top20.wfSampledBlogsSW$type <- "Sampled Blogs SW"
# 
# top20.wfCompleteBlogs <- head(blogsAll.wf, n = 20)
# top20.wfCompleteBlogs$type <- "Complete Blogs"
# top20.wfCompleteBlogsSW <- head(blogsAll.wf_sw, n = 20)
# top20.wfCompleteBlogsSW$type <- "Complete Blogs SW"
# 
# df.plotwf <- rbind(top20.wfCompleteBlogs,
#                    top20.wfCompleteBlogsSW,
#                    top20.wfSampledBlogs,
#                    top20.wfSampledBlogsSW)
# 
# tail(df.plotwf)
# windows()
# ggplot(df.plotwf, aes(x = word, y = frequency))  +
#     geom_bar(stat = "identity", fill = "darkred") +
#     facet_grid(~type) +
#     coord_flip() +
#     theme_gdocs() +
#     geom_text(aes(label = frequency), 
#               color = "white", hjust = 1.25, size = 5.0) +
#     ggtitle("Test")