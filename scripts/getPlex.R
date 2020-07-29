## Get all Plex info

getPlex <- function() {
    
    library(tidyverse);library(httr);library(lubridate);library(rdrop2)
    token <- '2CdDkLKF5xY27xxuxHB5'
    allTracks <- content(GET("http://192.168.1.99:32400/library/sections/5/search?type=10", 
                             add_headers("X-Plex-Token" = token))) %>% 
        magrittr::extract2('MediaContainer') %>% 
        magrittr::extract2('Metadata')
    
    ratedDF <- allTracks %>% 
        map_df(function(x) {
            as_tibble(x) %>% 
                select(any_of(c("grandparentTitle", "originalTitle", 
                                "parentTitle", "title", "userRating", "ratingKey")))
        }) %>% 
        select(albumArtist = grandparentTitle, 
               artist = originalTitle, 
               album = parentTitle, 
               track = title, 
               rating = userRating,
               key = ratingKey)
    
    return(ratedDF)
    
}
plex <- getPlex()
