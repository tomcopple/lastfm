## Add albums to Colllection: Singles

library(tidyverse);library(httr);library(lubridate);library(rdrop2)

source('scripts/getPlex.R')
plex <- getPlex(T)

token <- 'ABhPTJsJFC1CsCPKzzhb'

identity <- content(httr::GET(url = "http://192.168.1.99:32400/identity")) %>% 
    magrittr::extract2('MediaContainer') %>% 
    magrittr::extract2('machineIdentifier')

## Found link to the Collection here:
slug <- "http://192.168.1.99:32400/library/collections/51622/items"

## See what's there
collection <- content(httr::GET(slug, add_headers("X-Plex-Token" = token))) %>% 
    magrittr::extract2('MediaContainer') %>% 
    magrittr::extract2('Metadata') %>% 
    map_df(., function(x) {
        y <- data.frame(
            title = x$title,
            artist = x$parentTitle        
            )
        })
glimpse(collection)

## Create list of album keys to add to Collection, i.e. albums with just one track?
## Add remove anything over 15 minutes (e.g. Nike Run, 2 many djs etc)
addKeys <- plex %>% 
    select(albumArtist, artist, album, albumKey, duration) %>% 
    add_count(albumArtist, album) %>% 
    filter(n == 1, duration < 900) %>%
    pull(albumKey)

addKeys %>% 
    walk(function(x) {
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
