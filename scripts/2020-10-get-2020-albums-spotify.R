## Check albums/tracks in 2020 playlist and playcounts

library(tidyverse);library(spotifyr)
source(here::here('scripts', 'getLastfm.R'))

playlistID <- '37zcx3WiFSLso2UZHE7mbl'

## Spotify authentication
endpoint <- httr::oauth_endpoint(
    authorize = "https://accounts.spotify.com/authorize",
    access =    "https://accounts.spotify.com/api/token")

app <- httr::oauth_app(appname = "last2spot", key = "95bdfb9cb87841208145ede83a2dd878",secret = "2ada4a2188b7420d8833b2fe783e5c03")

spotAuth <- httr::oauth2.0_token(endpoint = endpoint, app = app,
                           scope = c('playlist-modify', 'playlist-modify-private'))

totalTracks <- httr::GET(url = str_c('https://api.spotify.com/v1/playlists/', playlistID, '/tracks'),
                         query = list(fields = "total"),
                  httr::config(token = spotAuth), encode = 'json') %>% 
    content(., as = 'text') %>% fromJSON() %>% pluck('total')

get2020albums <- function(x) {
    tracksRaw <- httr::GET(url = str_c('https://api.spotify.com/v1/playlists/', playlistID, '/tracks'),
                           query = list(
                               fields = "items(track(artists.name,album(name),track_number,name))",
                               limit = 100, offset = x),
                           httr::config(token = spotAuth), encode = 'json'
    )
    tracksReturn <- content(tracksRaw, as = 'text') %>% fromJSON() %>% pluck('items') %>% pluck('track') %>% as_tibble() %>% 
        mutate(album = purrr::flatten_chr(album)) %>% 
        mutate(artist = purrr::map_chr(artists, function(x) first(unlist(x))), .keep = 'unused') %>% 
        rename(track = name)
    i <- i + 100
    return(tracksReturn)
}

## Not sure this is best way but seems to work
i <- 0
j <- c()

while (i <= totalTracks) {
    j <- c(j, i)
    i <- i + 100
}

tracks <- map_df(j, get2020albums)
tracks

lastfm <- getLastfm(T)

trackCount <- tracks %>% 
    mutate(track_join = str_to_lower(track),
           artist_join = str_to_lower(artist)) %>% 
    left_join(lastfm %>% 
                  transmute(track_join = str_to_lower(track),
                            artist_join = str_to_lower(artist)) %>% 
                  count(artist_join, track_join),
              by = c('artist_join', 'track_join')) %>% 
    select(!contains('_join'))

saveRDS(trackCount, file = here::here('trackCount.rds'))
