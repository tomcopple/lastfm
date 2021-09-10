## get spotify playcounts from lastfm for specific playlists

library(tidyverse);library(httr);library(jsonlite)

source(here::here('scripts', 'getLastfm.R'))
source(here::here('scripts', 'getSlugs.R'))

# List of playlists -------------------------------------------------------
newSongs <- '6jGRKTY1lxycGw4ZVXm3DR'
newMusic <- "5vQN8Cq4bbav7Dt31mTwr6"


# Spotify auth ------------------------------------------------------------
endpoint <- httr::oauth_endpoint(
    authorize = "https://accounts.spotify.com/authorize",
    access =    "https://accounts.spotify.com/api/token")

app <- httr::oauth_app(appname = "last2spot", key = "95bdfb9cb87841208145ede83a2dd878",secret = "2ada4a2188b7420d8833b2fe783e5c03")

spotAuth <- httr::oauth2.0_token(endpoint = endpoint, app = app,
                                 scope = c('playlist-modify', 'playlist-modify-private'))


# Download Spotify Tracks -------------------------------------------------

## Choose playlist
playlist <- newMusic

## Can only get 100 tracks at a time, so need to know how many times to run it
totalTracks <- httr::GET(url = str_c('https://api.spotify.com/v1/playlists/', playlist, '/tracks'),
                         query = list(fields = "total"),
                         httr::config(token = spotAuth), encode = 'json') %>% 
    content(., as = 'text') %>% fromJSON() %>% pluck('total')

offsets <- seq.int(from = 0, to = floor(totalTracks/100))

## Define function to get tracks
## x is offset, i.e. starts at 1 and goes up (defined by i)
getTracks <- function(x) {
    tracksRaw <- httr::GET(url = str_c('https://api.spotify.com/v1/playlists/', playlist, '/tracks'),
                           query = list(
                               fields = "items(track(artists.name,album(name),track_number,name))",
                               limit = 100, offset = x*100),
                           httr::config(token = spotAuth), encode = 'json'
    )
    tracksReturn <- content(tracksRaw, as = 'text') %>% fromJSON() %>% pluck('items') %>% pluck('track') %>% as_tibble() %>% 
        mutate(album = purrr::flatten_chr(album)) %>% 
        mutate(artist = purrr::map_chr(artists, function(x) first(unlist(x))), .keep = 'unused') %>% 
        rename(track = name)
    return(tracksReturn)
}

tracks <- map_df(offsets, getTracks)
tracks

## Get Lastfm history
lastfm <- getLastfm(T) 

## Merge together
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
