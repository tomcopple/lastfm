## Check albums/tracks in 2020 playlist and playcounts

library(tidyverse);library(spotifyr);library(httr);library(jsonlite);library(rdrop2)
source(here::here("R", "bootstrap.R"))
source_project("R", "lib", "getLastfm.R")
source_project("R", "lib", "getSlugs.R")

playlistID <- '5vQN8Cq4bbav7Dt31mTwr6'
newSongs <- '6jGRKTY1lxycGw4ZVXm3DR'

## New Music id: "5vQN8Cq4bbav7Dt31mTwr6"

## Spotify authentication
endpoint <- httr::oauth_endpoint(
    authorize = "https://accounts.spotify.com/authorize",
    access =    "https://accounts.spotify.com/api/token")

app <- httr::oauth_app(appname = "last2spot", key = "95bdfb9cb87841208145ede83a2dd878",secret = "2ada4a2188b7420d8833b2fe783e5c03")

spotAuth <- httr::oauth2.0_token(endpoint = endpoint, app = app,
                           scope = c('playlist-modify', 'playlist-modify-private'))

totalTracks <- httr::GET(url = str_c('https://api.spotify.com/v1/playlists/', newSongs, '/tracks'),
                         query = list(fields = "total"),
                  httr::config(token = spotAuth), encode = 'json') %>% 
    content(., as = 'text') %>% fromJSON() %>% pluck('total')

getTracks <- function(x) {
    tracksRaw <- httr::GET(url = str_c('https://api.spotify.com/v1/playlists/', newSongs, '/tracks'),
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

tracks <- map_df(j, getTracks)
tracks

lastfm <- getLastfm(T) 

trackCount <- tracks %>% 
    mutate(track_join = getSlugs(track),
           artist_join = getSlugs(artist)) %>% 
    left_join(lastfm %>% 
                  transmute(track_join = getSlugs(track),
                            artist_join = getSlugs(artist)) %>% 
                  count(artist_join, track_join),
              by = c('artist_join', 'track_join')) %>% 
    select(!contains('_join'))

trackCount %>% arrange(desc(n)) %>% 
    select(artist, track, album, n)

trackCount %>% count(artist, wt = n, sort = T)
