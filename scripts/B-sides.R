## B-sides: find tracks that were initially included in annual playlists, but didn't make the cut
## How? Find days when other tracks that are in the playlist were played, and see what else was around

library(tidyverse);library(here);library(lubridate)

source(here::here('scripts', 'getLastfm.R'))

lastfm <- getLastfm(F)

## E.g. 2020
playlist2021 <- "0vdFNsbGqVvr7TcIY0nVsV" ## won't be the right id soon
playlist2020 <- "4GEYrbuyTRjAbihKAZ7iIe"
playlist2019 <- "54u2Bk9Rfp0bPcXZk3juTx"
playlist2018 <- "5Z0xb2Ox9e5KJIMpQHlD9l"
playlist2017 <- "5bx7hvsUh7R50ua4zEfrxA"
playlist2016 <- "0ClWgWedtZRRA59NojlkT1"
playlist <- playlist2021

# Spotify auth ------------------------------------------------------------
token <- spotifyr::get_spotify_access_token(
    client_id = "95bdfb9cb87841208145ede83a2dd878", client_secret = "2ada4a2188b7420d8833b2fe783e5c03"
)
endpoint <- httr::oauth_endpoint(
    authorize = "https://accounts.spotify.com/authorize",
    access =    "https://accounts.spotify.com/api/token")

app <- httr::oauth_app(appname = "last2spot", key = "95bdfb9cb87841208145ede83a2dd878",secret = "2ada4a2188b7420d8833b2fe783e5c03")

spotAuth <- httr::oauth2.0_token(endpoint = endpoint, app = app,
                                 scope = c('playlist-modify', 'playlist-modify-private'))

# Downloaded Spotify Playlist ---------------------------------------------

tracksRaw <- httr::GET(url = str_c('https://api.spotify.com/v1/playlists/', playlist, '/tracks'),
                       query = list(
                           fields = "items(track(artists.name,album(name),track_number,name))",
                           limit = 100),
                       httr::config(token = spotAuth), encode = 'json'
)
tracksReturn <- content(tracksRaw, as = 'text') %>% fromJSON() %>% pluck('items') %>% pluck('track') %>% as_tibble() %>% 
    mutate(album = purrr::flatten_chr(album)) %>% 
    mutate(artist = purrr::map_chr(artists, function(x) first(unlist(x))), .keep = 'unused') %>% 
    rename(track = name) %>% 
    unite(col = 'match', artist, track, sep = " ") %>% 
    transmute(match = getSlugs(match))


# Filter Lastfm -------------------------------------------------------

## Find days when 3 or more tracks from the playlist were played
playDates <- lastfm %>% 
    unite(artist, track, sep = " ", col = 'match') %>% 
    transmute(match = getSlugs(match),
           date = as_date(date)) %>% 
    inner_join(tracksReturn) %>% 
    # only keep one track play per day, otherwise includes albums played on repeat
    distinct(match, date) %>% 
    add_count(date) %>% 
    filter(n > 3) %>% 
    pull(date)

## Filter lastfm for those dates
leftovers <- lastfm %>% 
    mutate(date = as_date(date)) %>% 
    filter(date %in% playDates) %>% 
    unite(artist, track, sep = " ", col = 'match') %>% 
    mutate(match = getSlugs(match)) %>% 
    count(match, sort = T) %>% 
    filter(!match %in% tracksReturn$match) %>% 
    filter(n > 3)
leftovers

## In case you want to check why a song is there
inner_join(
    filter(lastfm, str_detect(track, 'Leimert')) %>% mutate(date = as_date(date)),
    lastfm %>% unite(artist, track, sep = " ", col = 'match') %>% 
        transmute(match = getSlugs(match),
                  date = as_date(date)) %>% 
        inner_join(tracksReturn) %>% 
        # only keep one track play per day, otherwise includes albums played on repeat
        distinct(match, date) %>% 
        add_count(date)
)
        

## Add to dataframe
# bsides <- leftovers
bsides <- bind_rows(bsides, leftovers) %>% 
    distinct(match, n)

removeEve <- function(x) {
    x <- x %>% 
        filter(
            str_detect(match, "cocomelon", negate = T),
            str_detect(match, "super simple", negate = T),
            str_detect(match, "duggee", negate = T),
            str_detect(match, "lullaby", negate = T),
            str_detect(match, "night garden", negate = T),
            str_detect(match, 'toddler tunes', negate = T),
            str_detect(match, 'pinkfong', negate = T)
        )
    return(x)
}

bsides <- removeEve(bsides)


# Create Spotify Playliust ------------------------------------------------

bsidesID <- "0whpMM7T98GMGFxknO2g0N"

getIDs <- function(track) {
    query <- list(q = track, type = 'track', market = 'GB', limit = 1, access_token = token)
    res <- httr::GET(url = "https://api.spotify.com/v1/search", query = query)
    resContent <- content(res)
    if (resContent$tracks$total > 0) {
        resID <- resContent$tracks$items[[1]]$id
    } else {
        resID <- NA
    }
    return(resID)
}
spotIDs <- bsides$match %>% map_chr(., getIDs)



## Then try to send them all to the playlist
httr::PUT(url = str_c('https://api.spotify.com/v1/playlists/', bsidesID, '/tracks'),
          body = list(uris = str_c('spotify:track:', na.omit(spotIDs))),
          httr::config(token = spotAuth), encode = 'json'
)
