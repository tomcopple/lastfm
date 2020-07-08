## Query Plex API?

getPlexRatings <- function(refresh = FALSE, write = FALSE) {
    library(tidyverse);library(httr);library(lubridate);library(rdrop2)
    token <- '2CdDkLKF5xY27xxuxHB5'
    
    ## Music is section 5, need to search '10' to get all tracks
    if(refresh) {
        allTracks <- content(GET("http://192.168.1.99:32400/library/sections/5/search?type=10", 
                                 add_headers("X-Plex-Token" = token))) %>% 
            magrittr::extract2('MediaContainer') %>% 
            magrittr::extract2('Metadata')
        
        ## Save tracks that have a rating
        rated <- allTracks %>% 
            purrr::discard(function(x) is.null(x$userRating))
        
        ## And keep artist, album, title, ratings
        ratedDF <- rated %>% 
            map_df(function(x) {
                as_tibble(x) %>% 
                    select(any_of(c("grandparentTitle", "originalTitle", "parentTitle", "title", "userRating")))
            }) %>% 
            select(albumArtist = grandparentTitle, artist = originalTitle, album = parentTitle, track = title, rating = userRating)
        
    } else {
        ratedDF <- rdrop2::drop_read_csv('R/lastfm/plexMasterRatings.csv') %>% 
            as_tibble()
    }
    
    
    ## Save one copy date-stamped today, and another master copy
    if(write) {
        write_csv(ratedDF, here::here('tempData', 
                                      str_c(lubridate::today(), "-plexRatings.csv")))
        rdrop2::drop_upload(file = here::here('tempData', 
                                              str_c(lubridate::today(), "-plexRatings.csv")),
                            path = 'R/lastfm/')
        
        write_csv(ratedDF, here::here('tempData', 'plexMasterRatings.csv'))
        rdrop2::drop_upload(file = here::here('tempData', 'plexMasterRatings.csv'), 
                            path = 'R/lastfm/')
    }
    
    
    ratings <- ratedDF %>% group_by(albumArtist, album) %>% 
        filter(n() > 1) %>% 
        summarise(avRat = mean(rating)/2) %>% 
        arrange(desc(avRat))
    print(ratings)
    .GlobalEnv$ratings <- ratings
    
}
