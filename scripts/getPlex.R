## Get all Plex info

getPlex <- function(refresh = FALSE) {
    
    library(tidyverse);library(httr);library(lubridate);library(rdrop2)
    
<<<<<<< HEAD
    numberOfTracks <- length(allTracks)
    tenPC <- round(numberOfTracks /10, 0)*10
    
    plex <- allTracks %>% 
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
    
    return(plex)
=======
    if (refresh) {
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
        
        write.csv(ratedDF, file = "plexDB.csv", 
                  row.names = FALSE, fileEncoding = "UTF-8")
        rdrop2::drop_upload(file = "plexDB.csv", path = "R/lastfm")
        
        return(ratedDF)    
    } else {
        
        ratedDF <- rdrop2::drop_read_csv(file = "R/lastfm/plexDB.csv") %>% 
            as_tibble()
        return(ratedDF)
        
    }
    
>>>>>>> fbb320385a5f5d60e420751636b1935b06db6591
    
}
