## Get all Plex info

getPlex <- function(refresh = FALSE) {
    
    library(tidyverse);library(httr);library(lubridate);library(rdrop2);library(jsonlite)
    
    if (refresh) {
        token <- 'ABhPTJsJFC1CsCPKzzhb'
        plexRaw <- content(GET("http://192.168.1.99:32400/library/sections/3/search?type=10", 
                                 add_headers("X-Plex-Token" = token)), type = 'text') %>% 
            jsonlite::fromJSON() %>% 
            pluck('MediaContainer') %>% 
            pluck('Metadata')
        
        plex <- plexRaw %>% select(
                albumArtist    = grandparentTitle, 
                   artist      = originalTitle, 
                   album       = parentTitle, 
                   track       = title, 
                   rating      = userRating,
                   key         = ratingKey,
                   artistKey   = grandparentRatingKey,
                   albumKey    = parentRatingKey,
                trackNum       = index,
                discNum        = parentIndex,
                year           = parentYear
            ) %>% 
            mutate(artist = ifelse(is.na(artist), albumArtist, artist)) %>% 
            as_tibble()
        
        write.csv(plex, file = here::here('tempData', 'plexDB.csv'), 
                  row.names = FALSE, fileEncoding = "UTF-8")
        rdrop2::drop_upload(file = here::here('tempData', "plexDB.csv"), 
                                              path = "R/lastfm")
        
        
    } else {
        
        plex <- rdrop2::drop_read_csv(file = "R/lastfm/plexDB.csv") %>% 
            as_tibble()
        
    }
    
    return(plex)
    
}
