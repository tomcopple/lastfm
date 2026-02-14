## B-sides: find tracks that were initially included in annual playlists, but didn't make the cut
## How? Find days when other tracks that are in the playlist were played, and see what else was around

library(tidyverse);library(here);library(lubridate);library(jsonlite);library(httr)

source(here::here("R", "bootstrap.R"))
source_project("R", "lib", "getLastfm.R")
source_project("R", "lib", "getSlugs.R")
lastfm <- getLastfm(T)

# Get list of annual playlists and IDs ----
# Spotify auth ------------------------------------------------------------
token <- spotifyr::get_spotify_access_token(
    client_id = "95bdfb9cb87841208145ede83a2dd878", client_secret = "2ada4a2188b7420d8833b2fe783e5c03"
)
endpoint <- httr::oauth_endpoint(
    authorize = "https://accounts.spotify.com/authorize",
    access =    "https://accounts.spotify.com/api/token")

app <- httr::oauth_app(appname = "last2spot", key = "95bdfb9cb87841208145ede83a2dd878",secret = "2ada4a2188b7420d8833b2fe783e5c03")

spotAuth <- httr::oauth2.0_token(endpoint = endpoint, app = app,
                                 scope = c('playlist-modify', 'playlist-modify-private',
                                           'playlist-read-private', 'playlist-read-collaborative'))

## Download list of playlists
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
        select_if(names(.) %in% c('name', 'id', 'images')) %>% 
        mutate(images = map_chr(images, function(x) {
            if(nrow(x) > 0) {
                x %>% pluck(2) %>% pluck(1)
            } else {
                ""
            }
            
        }))
    allPlaylists <- bind_rows(allPlaylists, getPlaylists) %>% 
        arrange(name)
}

annualPlaylists <- filter(allPlaylists, str_detect(name, "^\\d{4}$")) %>% 
    mutate(name = as.numeric(name)) %>% filter(name >= 2016)
annualPlaylists

# Downloaded songs for each playlist ---------------------------------------------

getAllSongs <- function(playlist) {
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
    
    return(tracksReturn)
}

annualSongs <- map_df(annualPlaylists$id, getAllSongs)

# Filter Lastfm -------------------------------------------------------

## Find days when 3 or more tracks from the playlist were played
playDates <- lastfm %>% 
    unite(artist, track, sep = " ", col = 'match') %>% 
    transmute(match = getSlugs(match),
           date = as_date(date)) %>% 
    inner_join(annualSongs) %>% 
    # only keep one track play per day, otherwise includes albums played on repeat
    distinct(match, date) %>% 
    add_count(date) %>% 
    filter(n > 3) %>% 
    pull(date)

## Filter lastfm for those dates
bsides <- lastfm %>% 
    mutate(date = as_date(date)) %>% 
    filter(date %in% playDates) %>% 
    unite(artist, track, sep = " ", col = 'match') %>% 
    mutate(match = getSlugs(match)) %>% 
    count(match, sort = T) %>% 
    filter(!match %in% annualSongs$match) %>% 
    filter(n > 3)
bsides

## In case you want to check why a song is there ----
inner_join(
    filter(lastfm, str_detect(track, 'Leimert')) %>% mutate(date = as_date(date)),
    lastfm %>% unite(artist, track, sep = " ", col = 'match') %>% 
        transmute(match = getSlugs(match),
                  date = as_date(date)) %>% 
        inner_join(annualSongs) %>% 
        # only keep one track play per day, otherwise includes albums played on repeat
        distinct(match, date) %>% 
        add_count(date)
)
        

## Clean/tidy
removeEve <- function(x) {
    x <- x %>% 
        filter(
            str_detect(match, "cocomelon", negate = T),
            str_detect(match, "super simple", negate = T),
            str_detect(match, "duggee", negate = T),
            str_detect(match, "lullaby", negate = T),
            str_detect(match, "night garden", negate = T),
            str_detect(match, 'toddler tunes', negate = T),
            str_detect(match, 'pinkfong', negate = T),
            str_detect(match, 'idina-menzel', negate = T)
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



## Then try to send them all to the playlist (max 100 per PUT)
for (i in 1:length(spotIDs) %/% 100 + 1) {
    min <- i * 100 - 99
    max <- i * 100
    miniList <- spotIDs[min:max] %>% na.omit()
    httr::POST(url = str_c('https://api.spotify.com/v1/playlists/', bsidesID, '/tracks'),
              body = list(uris = str_c('spotify:track:', na.omit(miniList))),
              httr::config(token = spotAuth), encode = 'json'
    )
}



