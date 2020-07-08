## Query Plex API?

library(tidyverse);library(httr)
token <- '2CdDkLKF5xY27xxuxHB5'

## Music is section 5, need to search '10' to get all tracks
allTracks <- content(GET("http://192.168.1.99:32400/library/sections/5/search?type=10", 
                   add_headers("X-Plex-Token" = token))) %>% 
    magrittr::extract2('MediaContainer') %>% 
    magrittr::extract2('Metadata')

## Explore all tracks
# allTracks[[3000]]$userRating
# allTracks[[1]]$userRating

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

write_csv(ratedDF, file.path("~", "Dropbox (Personal)", "R", "lastfm", 
                             str_c(lubridate::today(), "-plexRatings.csv")))

ratedDF %>% group_by(albumArtist, album) %>% 
    filter(n() > 1) %>% 
    summarise(avRat = mean(rating)/2) %>% 
    arrange(desc(avRat))
