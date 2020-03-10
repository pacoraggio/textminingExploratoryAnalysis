# name: plotcustomf.R
# sbj: Plotting Functions
# author: Paolo Coraggio
# create date: 04/03/2020

library(ggplot2)
library(ggthemes)
library(patchwork)


plotbar.wf <- function(df, title = "Word Frequency", topn = 10)
{
    df$word <- factor(df$word,
                      levels = unique(as.character(df$word)))
    p <- ggplot(df[1:topn,], aes(x = word, y = frequency))  +
        geom_bar(stat = "identity", fill = "darkred") +
        coord_flip() +
        theme_gdocs() +
        geom_text(aes(label = frequency), 
                  color = "white", hjust = 1.25, size = 5.0) +
        ggtitle(title)
    return(p)
}


plotbar.ngramf <- function(df, title = "Word Frequency", topn = 10)
{
    df <- rename(df, c(ngram.var = names(df)[1], frequency = names(df)[2]))
    
    df$ngram.var <- factor(df$ngram.var,
                      levels = unique(as.character(df$ngram.var)))
    p <- ggplot(df[1:topn,], aes(x = ngram.var, y = frequency))  +
        geom_bar(stat = "identity", fill = "darkred") +
        coord_flip() +
        theme_gdocs() +
        geom_text(aes(label = frequency), 
                  color = "white", hjust = 1.25, size = 5.0) +
        ggtitle(title)
    return(p)
}
