clean.corpus <- function(corpus)
{
    corpus <- tm_map(corpus, content_transformer(tryToLower))
    corpus <- tm_map(corpus, removeWords, custom.stopwords)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, stripWhitespace)
    corpus <- tm_map(corpus, removeNumbers)
    return(corpus)
}

clean.text <- function(lines)
{
    lines <- tolower(lines)
    lines <- gsub("[[:punct:]]", "", lines) # remove punctuation
    lines <- gsub("[[:digit:]]", "", lines) # remove digits
    lines <- gsub("\\s+", " ", str_trim(lines)) # remove extra whitespaces
    return(lines)
}
