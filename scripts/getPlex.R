## Get all Plex info

getPlex <- function(refresh = FALSE) {
    
    library(tidyverse);library(httr);library(lubridate);library(rdrop2)
    
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
        
        ratedDF <- rdrop2::drop_read_csv(file = "R/lastfm/plexDB.csv")
        return(ratedDF)
        
    }
    
    
}
