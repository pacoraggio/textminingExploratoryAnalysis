# name: wordfrequency
# sbj: functions calculating word frequency from dataframe
# author: Paolo Coraggio
# create date: 02/03/2020

library(tm)
library(dplyr)

word.frequency <- function(df.corpus, remove.stopwords = FALSE)
{
    custom.stopwords <- data.frame(word = stopwords('english'),
                                   lexicon = "mylexicon")
    
    df.wordfreq <- df.corpus %>%
        unnest_tokens(word, text) %>%
        {if(remove.stopwords) anti_join(., y = custom.stopwords) else .} %>%
        count(word, sort = TRUE) %>%
        rename(c(frequency = n))
    
    return(df.wordfreq)
}
