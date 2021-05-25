## Function to change everything to lower text, remove punctuation, replace spaces with dash
## Avoids some problems with joins %in% etc

getSlugs <- function(text) {
    
    library(stringr)
    text1 <- text %>% 
        str_to_lower() %>% 
        str_remove_all(pattern = "[[:punct:]]") %>% 
        str_replace_all(pattern = "&", "and") %>% 
        str_replace_all(pattern = "\\s{2,}", replacement = " ") %>% 
        str_trim(side = 'both') %>% 
        str_remove_all(pattern = "\\s\\(.*") %>% 
        str_replace_all(pattern = " ", replacement = "-")
    
    return(text1)
    
}

getSlugs <- function(x) {
    x1 <- stringr::str_to_lower(x)
    x2 <- stringr::str_replace_all(x1, "&", "and")
    x3 <- stringr::str_remove_all(x2, "[:punct:]")
    x4 <- stringr::str_replace_all(x3, "\\s{2,}", " ")
    x5 <- stringr::str_trim(x4, side = "both")
    ## Remove anything in brackets, e.g. (remastered) etc
    x6 <- stringr::str_remove_all(x5, "\\s\\(.*")
    return(x6)
}