# name: tidyNgrams.R
# sbj: functions for transform a corpus dataframe into data
#      structure containg n-grams
# type: library
# author: Paolo Coraggio
# create date: 02/03/2020

library(tidytext)
library(tidyr)
library(tm)
library(dplyr)

unnestbigrams <- function(df.corpus)
{
    df.bigrams <- df.corpus %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        drop_na()
    return(df.bigrams)
}


# news.bigramsseparated <- news.bigrams %>%
#     separate(bigram, c("word1", "word2"), sep = " ")
# 
# news.bigramsfiltered <- news.bigramsseparated %>%
#     filter(!word1 %in% stop_words$word) %>%
#     filter(!word2 %in% stop_words$word)

separate.bigrams <- function(df.bigrams)
{
    df.sepbigrams <- df.bigrams %>%
        separate(bigram, c("word1", "word2"), sep = " ")
    return(df.sepbigrams)
}

filter.bigrams <- function(df.splittedbigrams, custom.stopwords)
{
    df.filteredbigrams <- df.splittedbigrams %>%
        filter(!word1 %in% custom.stopwords) %>%
        filter(!word2 %in% custom.stopwords)
}

# recombining

# news.bigramsum <- news.bigramsfiltered %>%
#     drop_na() %>%
#     unite(bigram, word1, word2, sep = " ")

# news.bigramsum <- news.bigramsfiltered %>%
#     drop_na() %>%
#     unite(bigram, word1, word2, sep = " ")
unite.bigrams <- function(df.splitted)
{
    united.bigrams <- df.splitted %>%
        unite(bigram, word1, word2, sep = " ")
    return(united.bigrams)
}

# n.bigrams <- unnestbigrams(df.news)
# n.bigramsseparated <- separate.bigrams(n.bigrams)
# n.bigramsfiltered <- filter.bigrams(n.bigramsseparated, custom.stopwords$word)
# n.bigramsunited <- unite.bigrams(n.bigramsfiltered)

unnest.clened <- function(df.corpus, custom.stopwords)
{
    df.bigrams <- unnestbigrams(df.corpus)
    df.separated <- separate.bigrams(df.bigrams)
    df.filtered <- filter.bigrams(df.separated, custom.stopwords$word)
    df.bigramunited <- unite.bigrams(df.filtered)
    return(df.bigramunited)
}



# word.frequency <- function(df.corpus, remove.stopwords = FALSE)
# {
#     custom.stopwords <- data.frame(word = stopwords('english'),
#                                    lexicon = "mylexicon")
#     
#     df.wordfreq <- df.corpus %>%
#         unnest_tokens(word, text) %>%
#         {if(remove.stopwords) anti_join(., y = custom.stopwords) else .} %>%
#         count(word, sort = TRUE) %>%
#         rename(c(frequency = n))
#     
#     return(df.wordfreq)
# }

create.bigrams <- function(df.corpus)
{
    df.bigrams <- unnestbigrams(df.corpus)
    df.separated <- separate.bigrams(df.bigrams)
    df.bigramunited <- unite.bigrams(df.separated)
    return(df.bigramunited)
}

create.bigramsfreq <- function(df.text, remove.stopwords = FALSE)
{
    custom.stopwords <- data.frame(word = stopwords('english'),
                                   lexicon = "mylexicon")
    
    bigrams <- df.text %>%
        unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
        drop_na() %>%
        separate(bigram, c("word1","word2")) %>%
        {if(remove.stopwords)
            filter(.,!word1 %in% custom.stopwords$word,
                   !word2 %in% custom.stopwords$word) else .} %>%
        count(word1, word2, sort = TRUE) %>%
        unite(bigram, word1, word2, sep = " ")
}


create.trigramsfreq <- function(df.text, remove.stopwords = FALSE)
{
    custom.stopwords <- data.frame(word = stopwords('english'),
                                   lexicon = "mylexicon")
    
        trigrams <- df.text %>%
            unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
            drop_na() %>%
            separate(trigram, c("word1","word2","word3")) %>%
            {if(remove.stopwords)
                filter(.,!word1 %in% custom.stopwords$word,
                       !word2 %in% custom.stopwords$word,
                       !word3 %in% custom.stopwords$word) else .} %>%
            count(word1, word2, word3, sort = TRUE) %>%
            unite(trigram, word1, word2, word3, sep = " ")
}


# https://www.rdocumentation.org/packages/sentimentr/versions/2.7.1/topics/profanity
create.tetragramsfreq <- function(df.text, remove.stopwords = FALSE)
{
    custom.stopwords <- data.frame(word = stopwords('english'),
                                   lexicon = "mylexicon")
    
    tetrarams <- df.text %>%
        unnest_tokens(tetragram, text, token = "ngrams", n = 4) %>%
        drop_na() %>%
        separate(tetragram, c("word1","word2","word3","word4")) %>%
        {if(remove.stopwords)
            filter(.,!word1 %in% custom.stopwords$word,
                   !word2 %in% custom.stopwords$word,
                   !word3 %in% custom.stopwords$word,
                   !word4 %in% custom.stopwords$word) else .} %>%
        count(word1, word2, word3, word4, sort = TRUE) %>%
        unite(trigram, word1, word2, word3, word4, sep = " ")
}

create.pentagramsfreq <- function(df.text, remove.stopwords = FALSE)
{
    custom.stopwords <- data.frame(word = stopwords('english'),
                                   lexicon = "mylexicon")
    
    pentagrams <- df.text %>%
        unnest_tokens(pentagram, text, token = "ngrams", n = 5) %>%
        drop_na() %>%
        separate(pentagram, c("word1","word2","word3","word4","word5")) %>%
        {if(remove.stopwords)
            filter(.,!word1 %in% custom.stopwords$word,
                   !word2 %in% custom.stopwords$word,
                   !word3 %in% custom.stopwords$word,
                   !word4 %in% custom.stopwords$word,
                   !word5 %in% custom.stopwords$word) else .} %>%
        count(word1, word2, word3, word4, word5, sort = TRUE) %>%
        unite(trigram, word1, word2, word3, word4, word5, sep = " ")
}


create.trigramsfreqTidy <- function(df.text)
{
    custom.stopwords <- data.frame(word = stopwords('english'),
                                   lexicon = "mylexicon")
    
    total.trigrams <- df.text %>%
        unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
        drop_na() %>%
        separate(trigram, c("word1","word2","word3")) %>%
        filter(!word1 %in% custom.stopwords$word,
                   !word2 %in% custom.stopwords$word,
                   !word3 %in% custom.stopwords$word) %>%
        count(word1, word2, word3, sort = TRUE) %>%
        unite(trigram, word1, word2, word3, sep = " ")
}