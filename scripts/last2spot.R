## Get Playlist of tracks with more than 5 plays but not in the last year
library(tidyverse);library(httr);library(purrr);library(stringr);library(spotifyr)
library(here)
source(here::here('scripts', 'getLastfm.R'))

lastfm <- getLastfm(F)

## Filter for playcount > 5, not played in last year
getPlaylist <- function(playCount = 5, lastPlayed = 1, howMany = 20) {
    lastfm %>% 
        group_by(artist, track) %>% 
        summarise(n = n(), date = max(date)) %>% 
        ungroup() %>% 
        filter(n > playCount, date < lubridate::today() - lubridate::years(lastPlayed)) %>% 
        arrange(desc(n)) -> tracks
    
    if(howMany > nrow(tracks)) {
        howMany <- nrow(tracks)
    }
    
    playlist <- sample_n(tracks, howMany) %>% 
        print() %>% 
        unite(col = tracks, artist, track, sep = " ") %>% 
        select(-n, -date) %>% 
        magrittr::extract2('tracks')
    
    playlistID <- "1BBr03knQFBNoj3EUN2rpm"
    
    token <- spotifyr::get_spotify_access_token(
        client_id = "95bdfb9cb87841208145ede83a2dd878", client_secret = "2ada4a2188b7420d8833b2fe783e5c03"
    )
    endpoint <- oauth_endpoint(
        authorize = "https://accounts.spotify.com/authorize",
        access =    "https://accounts.spotify.com/api/token")
    app <- oauth_app(appname = "last2spot", key = "95bdfb9cb87841208145ede83a2dd878",secret = "2ada4a2188b7420d8833b2fe783e5c03")
    spotAuth <- oauth2.0_token(endpoint = endpoint, app = app,
                               scope = c('playlist-modify', 'playlist-modify-private'))
    
    
    spotIDs <- playlist %>% map_chr(function(x) {
        query <- list(
            q = x,
            type = "track",
            market = "GB",
            limit = 1,
            access_token = token
        )
        res <- httr::GET(url = "https://api.spotify.com/v1/search", query = query)
        resContent <- content(res)
        if(resContent$tracks$total > 0) {
            resID <- resContent$tracks$items[[1]]$id
        } else {
            resID <- NA
        }
        return(resID)
    })
    
    res <- httr::PUT(
        url = stringr::str_c(
            "https://api.spotify.com/v1/playlists/", playlistID, "/tracks", sep = ""
        ), 
        body = list(
            uris = str_c("spotify:track:", na.omit(spotIDs), sep = "")
        ),
        config(token = spotAuth), encode = "json"
    )
    
    
    jsonlite::fromJSON(content(res, as = "text", encoding = "UTF-8"), 
                       flatten = TRUE)
    

    
    }
playlist <- getPlaylist(6, 3, 25)




