# name: tmEDAScript
# sbj: tidyverseApproach
# author: Paolo Coraggio
# create date: 02/03/2020

rm(list = ls())

options(stringsAsFactors = FALSE)
Sys.setlocale('LC_ALL', 'C')

library(dplyr)
library(tidytext)
library(janeaustenr)
library(stringr)
library(ggplot2)
library(tictoc)

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


df.news <- createSampledDfText('./en_US/en_US.news.txt', 0.015, book = "news")
df.blogs <- createSampledDfText('./en_US/en_US.blogs.txt', 0.015, book = "blogs")
df.twitter <- createSampledDfText('./en_US/en_US.twitter.txt', 0.0075, book = "twitter")

df.complete <- rbind(df.news, df.blogs, df.twitter)
df.complete$doc_id <- 1:nrow(df.complete)

save(df.news, file = "dfnews.RData")
save(df.blogs, file = "dfblogs.RData")
save(df.twitter, file = "dftwitter.RData")
save(df.complete, file = "dfcomplete.RData")

# sum(nchar(df.news$text))
# sum(nchar(df.blogs$text))
# sum(nchar(df.twitter$text))

#names(df.news)
## Cleaning Text

df.twitter$text <- clean.text(df.twitter$text)
df.news$text <- clean.text(df.news$text)
df.blogs$text <- clean.text(df.blogs$text)
df.complete$text <- clean.text(df.complete$text)

# a2 <- grep("[^a-zA-Z]will",df.news$text, value = TRUE)
# mstopwords <- get_stopwords(language = "en", source = "snowball")

library(tm)
custom.stopwords <- data.frame(word = stopwords('english'),
                               lexicon = "mylexicon")

head(custom.stopwords)

news.wf <- word.frequency(df.corpus = df.news, remove.stopwords = TRUE)
blogs.wf <- word.frequency(df.corpus = df.blogs, remove.stopwords = TRUE)
twitter.wf <- word.frequency(df.corpus = df.twitter, remove.stopwords = TRUE)
complete.wf <- word.frequency(df.corpus = df.complete, remove.stopwords = TRUE)

p.news <- plotbar.wf(news.wf, "News")
p.blogs <- plotbar.wf(blogs.wf, "Blogs")
p.twitter <- plotbar.wf(twitter.wf, "Twitter")
p.complete <- plotbar.wf(complete.wf, "Total")

windows()
((p.news + p.blogs + p.twitter) / p.complete)


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


## N-Grams

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

# forse drop_na va qui
news.bigrams <- df.news %>% 
    unnest_tokens(bigram, text, token = "ngrams", n =2) %>%
    drop_na()


sum(news.bigrams == n.bigrams)/3
head(news.bigrams$bigram)

blogs.bigrams <- df.blogs %>% 
    unnest_tokens(bigram, text, token = "ngrams", n =2,
                  collapse = TRUE) 

twitter.bigrams <- df.twitter %>% 
    unnest_tokens(bigram, text, token = "ngrams", n =2,
                  collapse = TRUE) 

# Counting and filtering n-grams

news.bigrams %>%
    count(bigram, sort = TRUE)

n.bigrams %>%
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

news.bigramsum <- news.bigramsfiltered %>%
    drop_na() %>%
    unite(bigram, word1, word2, sep = " ")

sum(is.na(news.bigramsfiltered))

## Analysing bigrams

# A bigram can also be treated as a term in a document in the same way that we
# trated individual words

# r <- news.bigramsun
# r$doc_id = 1
# head(r)
n.bigrams <- unnestbigrams(df.news)
n.bigramsseparated <- separate.bigrams(n.bigrams)
n.bigramsfiltered <- filter.bigrams(n.bigramsseparated, custom.stopwords$word)
n.bigramsunited <- unite.bigrams(n.bigramsfiltered)

blogs.cbigrams <- unnest.clened(df.blogs, custom.stopwords)
twitter.cbigrams <- unnest.clened(df.twitter, custom.stopwords)

total.cbigrams <- unnest.clened(df.complete, custom.stopwords)

remove(n.bigramsfiltered, n.bigramsseparated, n.bigrams)


test <- news.bigramsum %>%
    count(book, bigram) %>%
    bind_tf_idf(bigram, book, n) %>%
    arrange(desc(tf))

test2 <- n.bigramsunited %>%
    count(book, bigram) %>%
    bind_tf_idf(bigram, book, n) %>%
    arrange(desc(tf))

test.blogs <- blogs.cbigrams  %>%
    count(book, bigram) %>%
    bind_tf_idf(bigram, book, n) %>%
    arrange(desc(tf))

test.twitter <- twitter.cbigrams  %>%
    count(book, bigram) %>%
    bind_tf_idf(bigram, book, n) %>%
    arrange(desc(tf))

length(unique(total.cbigrams$doc_id))


test.total <- total.cbigrams  %>%
    count(book, bigram) %>%
    bind_tf_idf(bigram, book, n) %>%
    arrange(desc(tf))

test.total <- total.cbigrams  %>%
    count(bigram) %>%
    arrange(desc(n))

tail(test.total, n = 15)

## 3-grams
## Visualizing a network of bigrams with ggraph
mgrams <- str_c("gram",3)
a1 <- unnest_tokens(df.news, grams, text, token = "ngrams", n = 3)
names(a1)[3] <- mgrams

nn <- 4
mgrams <- str_c("gram",nn)
a1 <- unnest_tokens(df.news, grams, text, token = "ngrams", n = nn)
names(a1)[3] <- mgrams
head(a1)

remove(a1)
nn <- 3
mgrams <- str_c("gram",nn)
a1 <- unnest_tokens(df.news, grams, text, token = "ngrams", n = nn)
names(a1)[3] <- mgrams
head(a1)

wd <- c()
for(k in 1:nn)
{
    wd <- c(wd, str_c("word",k))
}   

names(df.complete)
total.trigrams <- df.complete %>%
    unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
    drop_na() %>%
    separate(trigram, c("word1","word2","word3")) %>%
    filter(!word1 %in% custom.stopwords$word,
           !word2 %in% custom.stopwords$word,
           !word3 %in% custom.stopwords$word) %>%
    count(word1, word2, word3, sort = TRUE) %>%
    unite(trigram, word1, word2, word3, sep = " ")

head(total.trigrams)

plotbar.ngramf(total.trigrams, ngram = "trigrams", topn = 15)

total.trigrams[,1]
ggplot(total.trigrams[1:10,], aes())



ptrigrams <- plotbar.ngramf(total.trigrams, title = "Trigrams Frequency")

names(df.news)

total.tetragrams <- df.complete %>%
    unnest_tokens(tetragram, text, token = "ngrams", n = 4) %>%
    drop_na() %>%
    separate(tetragram, c("word1","word2","word3", "word4")) %>%
    filter(!word1 %in% custom.stopwords$word,
           !word2 %in% custom.stopwords$word,
           !word3 %in% custom.stopwords$word,
           !word4 %in% custom.stopwords$word) %>%
    count(word1, word2, word3, word4, sort = TRUE) %>%
    unite(tetragram, word1, word2, word3, word4, sep = " ")

head(total.tetragrams)

ptri <- plotbar.wf(total.trigrams)
ptetra <- plotbar.wf(total.tetragrams)
ptetra <- plotbar.ngramf(total.tetragrams, ngram = "tetra")
windows()
ptetra

total.pentagrams <- df.complete %>%
    unnest_tokens(pentagrams, text, token = "ngrams", n = 5) %>%
    drop_na() %>%
    separate(pentagrams, c("word1","word2","word3", "word4", "word5")) %>%
    filter(!word1 %in% custom.stopwords$word,
           !word2 %in% custom.stopwords$word,
           !word3 %in% custom.stopwords$word,
           !word4 %in% custom.stopwords$word,
           !word5 %in% custom.stopwords$word) %>%
    count(word1, word2, word3, word4, word5, sort = TRUE) %>%
    unite(pentagram, word1, word2, word3, word4, word5, sep = " ")



head(total.trigrams, n = 20)
head(total.tetragrams, n = 20)
head(total.pentagrams, n = 20)
library(ggraph)


### working with trigrams
library(igraph)

a1 <- unnest.clened(df.complete, custom.stopwords)
a2 <- separate.bigrams(a1)
a3 <- filter.bigrams(a2, custom.stopwords)
a3.count <- a3 %>%
    count(word1, word2, sort = TRUE)

bigram_graph <- a3.count %>%
    filter(n > 3) %>%
    graph_from_data_frame()

windows()
bigram_graph

set.seed(2020)
windows()
ggraph(bigram_graph, layout = "fr") +
    geom_edge_link() +
    geom_node_point() +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1)