# name: createSampledText
# author: Paolo Coraggio
# create date: 02/03/2020

createSampledText <- function(original, sample.percentage = 0.5)
{
    if(!file.exists(original))
    {
        print("no file")
        return(NULL)
    } 
    f <- file(original, "rb")
    original.text <- readLines(f, encoding = "UTF-8")
    close(f)
    n.lines <- sort(sample(1:length(original.text),
                           as.integer(length(original.text) * sample.percentage),
                           replace = FALSE))
    sampled.text <- original.text[n.lines]
    return(sampled.text)
}

createSampledDfText <- function(original, sample.percentage = 0.5)
{
    if(!file.exists(original))
    {
        print("no file")
        return(NULL)
    } 
    f <- file(original, "rb")
    original.text <- readLines(f, encoding = "UTF-8", skipNul = TRUE)
    close(f)
    n.lines <- sort(sample(1:length(original.text),
                           as.integer(length(original.text) * sample.percentage),
                           replace = FALSE))
    sampled.text <- original.text[n.lines]
    return(data.frame(doc_id = 1:as.integer(length(original.text) * sample.percentage),
                      text = sampled.text,
                      stringsAsFactors = FALSE))
}
