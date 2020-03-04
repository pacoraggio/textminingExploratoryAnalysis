# name: tmEDAScript
# sbj: tidyverseApproach
# author: Paolo Coraggio
# create date: 02/03/2020

rm(list = ls())

options(stringsAsFactors = FALSE)
# Sys.setlocale('LC_ALL', 'C')

library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(tictoc)

source("createSampledText.R")
source("plotcustomf.R")
a1 <- read.table('./en_US/summary.txt', 
                 col.names = c("lines",
                               "words",
                               "characters",
                               "bytes",
                               "source"),
                 nrows = 3)

a1 <- mutate(a1, 
             Mb = round(bytes*0.00000095367432,2))
    
a1 <- a1[,c(5,1,2,3,6)]


# en_US.blogs.txt made of 899288 lines
# en_US.twitter.txt made of 2360148 lines
# en_US.news.txt made of 1010242 lines

tic()
sample.news <- createSampledText('./en_US/en_US.news.txt', 0.01)
sample.blogs <- createSampledText('./en_US/en_US.blogs.txt', 0.01)
sample.twitter <- createSampledText('./en_US/en_US.twitter.txt', 0.01)
toc()

df.news <- data.frame(n_row = 1:length(sample.news),
                       text = sample.news, stringsAsFactors = FALSE)

df.blogs <- data.frame(n_row = 1:length(sample.blogs),
                      text = sample.blogs, stringsAsFactors = FALSE)

df.twitter <- data.frame(n_row = 1:length(sample.twitter),
                      text = sample.twitter, stringsAsFactors = FALSE)

tic()

df.news <- createSampledDfText('./en_US/en_US.news.txt', 0.01)
df.blogs <- createSampledDfText('./en_US/en_US.blogs.txt', 0.01)
df.twitter <- createSampledDfText('./en_US/en_US.twitter.txt', 0.01)


## Cleaning Text
clean.text <- function(lines)
{
    lines <- tolower(lines)
    lines <- gsub("[[:punct:]]", "", lines) # remove punctuation
    lines <- gsub("[[:digit:]]", "", lines) # remove digits
    lines <- gsub("\\s+", " ", str_trim(lines)) # remove extra whitespaces
    return(lines)
}

df.twitter$text <- clean.text(df.twitter$text)
df.news$text <- clean.text(df.news$text)
df.blogs$text <- clean.text(df.blogs$text)

# a2 <- grep("[^a-zA-Z]will",df.news$text, value = TRUE)

mstopwords <- get_stopwords(language = "en", source = "snowball")

library(tm)
custom.stopwords <- data.frame(word = stopwords('english'),
                               lexicon = "mylexicon")

head(custom.stopwords)

tidyNews <- df.news %>% 
    unnest_tokens(word, text) %>%
    anti_join(custom.stopwords)


tidyBlogs <- df.blogs %>% 
    unnest_tokens(word, text) %>%
    anti_join(custom.stopwords)

tidyTwitter <- df.twitter %>% 
    unnest_tokens(word, text) %>%
    anti_join(custom.stopwords)

# calculating and plotting the word frequency

fw.news <- tidyNews %>% 
    count(word, sort = TRUE) %>%
    rename(c(frequency = n))

fw.blogs <- tidyBlogs %>% 
    count(word, sort = TRUE) %>%
    rename(c(frequency = n))

fw.twitter <- tidyTwitter %>% 
    count(word, sort = TRUE) %>%
    rename(c(frequency = n))


fw.news$word <- factor(fw.news$word, 
                       levels = unique(as.character(fw.news$word)))

fw.blogs$word <- factor(fw.blogs$word, 
                       levels = unique(as.character(fw.blogs$word)))

fw.twitter$word <- factor(fw.twitter$word, 
                        levels = unique(as.character(fw.twitter$word)))


p1 <- plotbar.wf(fw.news, "News Word Freqeuncy")
p2 <- plotbar.wf(fw.blogs, "Blogs Word Freqeuncy")
p3 <- plotbar.wf(fw.twitter, "Twitter Word Freqeuncy")

windows()
p1 + p2 + p3


remove_reg <- "&amp;|&lt|&gt;"
tidyTwitter1 <- df.twitter %>% 
    filter(!str_detect(text, "^RT")) %>%
    mutate(text = str_remove_all(text, remove_reg)) %>%
    unnest_tokens(word, text, token = "tweets") %>%
    filter(!word %in% stop_words$word,
           !word %in% str_remove_all(stop_words, "'"),
           str_detect(word,"[a-z]"))

l2 <- df.twitter[str_detect(df.twitter$text, "^RT"),]$text

head(l3)
head(l3)

t1 <- tidyTwitter %>% 
    count(word, sort = TRUE)

tidyTwitter1 %>% 
    count(word, sort = TRUE)


# Relationships between words: n-grams and correlations
# Word Frequency consider words as individual units however our goal is to
# examining relationships between words i.e. which words tend to follow
# others immediately or that tend to co-occur within the same document

# the tiditext approach includes the `token = "ngrams"` argument which tokenizes
# which pairs of adjacent words rather than by individual ones

## Tokenizing by n-gram

# bigrams
# df.news[df.news$text == "",]
# df.news <- df.news[-df.news[df.news$text == "",]$doc_id,]

news.bigrams <- df.news %>% 
    unnest_tokens(bigram, text, token = "ngrams", n =2,
                  collapse = TRUE) 

blogs.bigrams <- df.blogs %>% 
    unnest_tokens(bigram, text, token = "ngrams", n =2,
                  collapse = TRUE) 

twitter.bigrams <- df.twitter %>% 
    unnest_tokens(bigram, text, token = "ngrams", n =2,
                  collapse = TRUE) 

# Counting and filtering n-grams

news.bigrams %>%
    count(bigram, sort = TRUE)

blogs.bigrams %>%
    count(bigram, sort = TRUE)

twitter.bigrams %>%
    count(bigram, sort = TRUE)

# Since I did not clean the word  list with any stopword, at the top of the list I 
# get a lot of "naive" correspondences. Using `separate()` function we can introduce
# and filter for stop words. 

library(tidyr)

news.bigramsseparated <- news.bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")

news.bigramsfiltered <- news.bigramsseparated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

blogs.bigramsseparated <- blogs.bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")

blogs.bigramsfiltered <- blogs.bigramsseparated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)

twitter.bigramsseparated <- twitter.bigrams %>%
    separate(bigram, c("word1", "word2"), sep = " ")

twitter.bigramsfiltered <- twitter.bigramsseparated %>%
    filter(!word1 %in% stop_words$word) %>%
    filter(!word2 %in% stop_words$word)


# new bigram counts:
news.bigramscounts <- news.bigramsfiltered %>%
    count(word1, word2, sort = TRUE)

blogs.bigramscounts <- blogs.bigramsfiltered %>%
    count(word1, word2, sort = TRUE)

twitter.bigramscounts <- twitter.bigramsfiltered %>%
    count(word1, word2, sort = TRUE)

# recombining

news.bigramsun <- news.bigramsfiltered %>%
    unite(bigram, word1, word2, sep = " ")


## Analysing bigrams

# A bigram can also be treated as a term in a document in the same way that we
# trated individual words

r <- news.bigramsun
r$doc_id = 1
head(r)
test <- r %>%
    count(doc_id, bigram) %>%
    bind_tf_idf(bigram, doc_id, n) %>%
    arrange(desc(tf))

arrange(test, desc(n))

news.bigram_tf_idf <- news.bigramsun %>%
    count(doc_id, bigram) %>%
    bind_tf_idf(bigram, doc_id, n) %>%
    arrange(desc(tf))

head(news.bigram_tf_idf)
?bind_tf_idf
a <- grep("orange county", df.news$text, ignore.case = TRUE, value = TRUE)