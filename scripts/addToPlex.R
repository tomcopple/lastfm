## Try making Plex playlist

library(tidyverse);library(httr);library(lubridate);library(rdrop2)

source('scripts/getPlex.R')
source('scripts/getLastfm.R')

token <- '2CdDkLKF5xY27xxuxHB5'

identity <- content(httr::GET(url = "http://192.168.1.99:32400/identity")) %>% 
    magrittr::extract2('MediaContainer') %>% 
    magrittr::extract2('machineIdentifier')

slug <- "http://192.168.1.99:32400/playlists/104340/items"

## Get track rating
# queryString <- str_c("server://", identity, "/com.plexapp.plugins.library/library/metadata/", trackKey)
# 
# finalPut <- httr::PUT(
#     url = slug,
#     add_headers("X-Plex-Token" = token),
#     query = list(
#         uri = queryString
#         )
# )
# 
lastfm <- getLastfm(T)
plex <- getPlex(refresh = TRUE)

playlist <- lastfm %>% 
    count(artist, track) %>% 
    right_join(plex %>% 
                   mutate(artist = ifelse(is.na(artist), albumArtist, artist))) %>% 
    filter(is.na(rating)) %>% 
    filter(n > 10) %>% 
    sample_n(size = 25)
playlist

## Want to delete everything first?
current <- content(httr::GET(slug, add_headers("X-Plex-Token" = token))) %>% 
    magrittr::extract2('MediaContainer') %>% 
    magrittr::extract2('Metadata') %>% 
    map_chr(., function(x) {x$ratingKey})

current %>% 
    map(function(x) {
        httr::DELETE(url = slug, add_headers("X-Plex-Token" = token),
                     query = list(
                         ratingKey = x
                     ))
    })

## Then add playlist
playlist %>% 
    magrittr::extract2('key') %>% 
    walk(function(x) {
        print(x)
        trackKey <- x
        queryString <- str_c("server://", identity,
                             "/com.plexapp.plugins.library/library/metadata/",
                             trackKey)

        finalPut <- httr::PUT(
            url = slug,
            add_headers("X-Plex-Token" = token),
            query = list(
                uri = queryString
            )
        )
    })
