## get spotify playcounts from lastfm for specific playlists

library(tidyverse);library(httr);library(jsonlite)

source(here::here('scripts', 'getLastfm.R'))
source(here::here('scripts', 'getSlugs.R'))

## Get Lastfm history
lastfm <- getLastfm(T) 



# Spotify auth ------------------------------------------------------------
endpoint <- httr::oauth_endpoint(
    authorize = "https://accounts.spotify.com/authorize",
    access =    "https://accounts.spotify.com/api/token")

app <- httr::oauth_app(appname = "last2spot", key = "95bdfb9cb87841208145ede83a2dd878",secret = "2ada4a2188b7420d8833b2fe783e5c03")

spotAuth <- httr::oauth2.0_token(endpoint = endpoint, app = app,
                                 scope = c('playlist-modify', 'playlist-modify-private',
                                           'playlist-read-private', 'playlist-read-collaborative'))



# Get list of playlists ---------------------------------------------------

howManyPlaylists <- httr::GET(url = "https://api.spotify.com/v1/users/tomcopple/playlists",
                              httr::config(token = spotAuth), encode = 'json') %>% 
    content(., as = 'text') %>% fromJSON() %>% pluck('total')

allPlaylists <- data.frame()
reps <- howManyPlaylists %/% 50 + 1

for (i in 1:reps) {
    getPlaylists <- httr::GET(url = "https://api.spotify.com/v1/users/tomcopple/playlists",
                              httr::config(token = spotAuth), encode = 'json',
                              query = list(limit = 50, offset = 50 * (i - 1))) %>% 
        content(., as = 'text') %>% fromJSON() %>% 
        pluck('items') %>% 
        select(name, id)
    allPlaylists <- bind_rows(allPlaylists, getPlaylists)
}


# Download Spotify Tracks -------------------------------------------------

## Choose playlist ----

playlist <- "New Music"
if (playlist %in% allPlaylists$name) {
    playlistID <- filter(allPlaylists, name == playlist) %>% pull(id)
} else {
    print(glue::glue('Playlist "{playlist}" not found'))
}

## Can only get 100 tracks at a time, so need to know how many times to run it
totalTracks <- httr::GET(url = str_c('https://api.spotify.com/v1/playlists/', playlistID, '/tracks'),
                         query = list(fields = "total"),
                         httr::config(token = spotAuth), encode = 'json') %>% 
    content(., as = 'text') %>% fromJSON() %>% pluck('total')

offsets <- seq.int(from = 0, to = floor((totalTracks-1)/100))

## Define function to get tracks
## x is offset, i.e. starts at 1 and goes up (defined by i)
getTracks <- function(x) {
    tracksRaw <- httr::GET(url = str_c('https://api.spotify.com/v1/playlists/', playlistID, '/tracks'),
                           query = list(
                               fields = "items(track(artists.name,album(name),track_number,name, id))",
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


## Some manual changes we have to do here
tracks <- tracks %>% 
    mutate(artist = case_when(
        artist == 'Bruno Mars' ~ 'Silk Sonic',
        artist == 'Arthur Jeffes' ~ 'Penguin Cafe',
        str_detect(artist, 'The War') ~ 'The War on Drugs',
        TRUE ~ artist
    )) %>% 
    filter(str_detect(track, 'caprisongs interlude', negate = TRUE))

## Merge together
trackCount <- tracks %>% 
    mutate(track_join = getSlugs(track),
           artist_join = getSlugs(artist)) %>% 
    left_join(lastfm %>% 
                  transmute(track_join = getSlugs(track),
                            artist_join = getSlugs(artist)) %>% 
                  count(artist_join, track_join),
              by = c('artist_join', 'track_join')) %>% 
    mutate(n = replace_na(n, 0)) %>% 
    select(!contains('_join'))

trackCount

### Top tracks ----
trackCount %>% arrange(desc(n)) %>% 
    select(artist, track, album, n)

### Bottom tracks ----
trackCount %>% arrange(n) %>% 
    select(artist, track, album, n)

### Count tracks by artist ----
trackCount %>% count(artist, wt = n, sort = T)

### Album with the most complete plays ----
trackCount %>% group_by(artist, album) %>% summarise(minPlay = min(n)) %>% 
    arrange(desc(minPlay)) %>% 
    filter(str_detect(album, 'Mirror', negate = TRUE))

### Lowest played song per artist ----
trackCount %>% group_by(artist, album) %>% 
    arrange(n) %>% 
    slice(1) %>% 
    ungroup()  %>% 
    slice_sample(n = 10) %>% 
    arrange(n)



# Send to Spotify Playlist ------------------------------------------------

## Going to use the same old playlist called R as can't be bothered to start a new one
## But NB need the track ID from Spotify

## 1. minPlays ----
minPlays <- trackCount %>% 
    group_by(artist, album) %>% 
    arrange(n) %>% 
    ## Filter for lowest plays
    top_n(-1) %>% 
    ## If more than one per artist/album just take random
    slice_sample(n = 1) %>% 
    ## get spotify ID
    pull(id)

## 2. top10 ----
top10 <- trackCount %>% 
    top_n(10) %>% 
    ## can't quite figure out best way of doing this
    ## Might have more than ten, but always want the most played anyway
    ## Just take random sample for now, even if you miss the max n
    slice_sample(n = 10) %>% 
    pull(id)

    
## Then try to send them all to the playlist
httr::PUT(url = str_c('https://api.spotify.com/v1/playlists/', "1BBr03knQFBNoj3EUN2rpm", '/tracks'),
              body = list(uris = str_c('spotify:track:', na.omit(minPlays))),
              httr::config(token = spotAuth), encode = 'json'
    )
