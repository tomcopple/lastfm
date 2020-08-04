## Get all Plex info

getPlex <- function(refresh = FALSE) {
    
    library(tidyverse);library(httr);library(lubridate);library(rdrop2);library(jsonlite)
    
    if (refresh) {
        token <- '2CdDkLKF5xY27xxuxHB5'
        allTracks <- content(GET("http://192.168.1.99:32400/library/sections/5/search?type=10", 
                                 add_headers("X-Plex-Token" = token)), type = 'text') %>% 
            jsonlite::fromJSON() %>% 
            magrittr::extract2('MediaContainer') %>% 
            magrittr::extract2('Metadata')%>% 
            select(
                albumArtist = grandparentTitle, 
                   artist      = originalTitle, 
                   album       = parentTitle, 
                   track       = title, 
                   rating      = userRating,
                   key         = ratingKey,
                   artistKey   = grandparentRatingKey,
                   albumKey    = parentRatingKey
            )
        
        write.csv(allTracks, file = "plexDB.csv", 
                  row.names = FALSE, fileEncoding = "UTF-8")
        rdrop2::drop_upload(file = "plexDB.csv", path = "R/lastfm")
        
        return(allTracks)    
        
    } else {
        
        allTracks <- rdrop2::drop_read_csv(file = "R/lastfm/plexDB.csv") %>% 
            as_tibble()
        return(allTracks)
        
    }
}
