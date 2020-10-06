## Create new playlist containing songs with half ratings. 

library(tidyverse);library(httr);library(lubridate);library(rdrop2)

halfies <- getPlex(T) %>% 
    filter(rating %% 2 == 1)

token <- '2CdDkLKF5xY27xxuxHB5'

identity <- content(httr::GET(url = "http://192.168.1.99:32400/identity")) %>% 
    magrittr::extract2('MediaContainer') %>% 
    magrittr::extract2('machineIdentifier')


## Need to create the playlist in Plex and find the ID to enter here:
slug <- "http://192.168.1.99:32400/playlists/108022/items"


## Need to delete everything before adding new tracks (as want to remove any new ratings)
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
halfies %>% 
    dplyr::pull(key) %>% 
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
