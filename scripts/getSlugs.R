## Function to change everything to lower text, remove punctuation, replace spaces with dash
## Avoids some problems with joins %in% etc

getSlugs <- function(text) {
    
    library(stringr)
    text1 <- text %>% 
        ## Remove any accents, cedillas etc (need to do this before lower case)
        iconv(from = 'UTF-8', to = "ASCII//TRANSLIT") %>% 
        str_to_lower() %>% 
        str_remove_all(pattern = "[[:punct:]]") %>% 
        str_replace_all(pattern = "&", "and") %>% 
        str_replace_all(pattern = "\\s{2,}", replacement = " ") %>% 
        ## Remove anything in brackets e.g. (remastered) etc
        str_remove_all(pattern = "\\s\\(.*") %>% 
        ## and remove any "feat."
        str_remove_all(pattern = "feat.*") %>% 
        str_trim(side = 'both') %>% 
        str_replace_all(pattern = " ", replacement = "-")
    
    return(text1)
    
}

