# name: plotcustomf.R
# sbj: Plotting Functions
# author: Paolo Coraggio
# create date: 04/03/2020

library(ggplot2)
library(ggthemes)
library(patchwork)


plotbar.wf <- function(df, title = "Word Frequency")
{
    df$word <- factor(df$word,
                      levels = unique(as.character(df$word)))
    p <- ggplot(df[1:10,], aes(x = word, y = frequency))  +
        geom_bar(stat = "identity", fill = "darkred") +
        coord_flip() +
        theme_gdocs() +
        geom_text(aes(label = frequency), 
                  color = "white", hjust = 1.25, size = 5.0) +
        ggtitle(title)
    return(p)
}
