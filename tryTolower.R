# name: tryTolower
# author: Ted Kwartler
# book: Text Mining in Practice with R
# create date: 02/03/2020

tryToLower <- function(x)
{
    # return NA when there is an error
    y = NA
    # tryCatch error
    try_error = tryCatch(tolower(x), error = function(e) e)
    # if not an error
    if(!inherits(try_error, 'error'))
        y = tolower(x)
    return(y)
}