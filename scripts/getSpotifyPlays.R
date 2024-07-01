## get spotify playcounts from lastfm for specific playlists

library(tidyverse);library(httr);library(jsonlite);library(httr2)

source(here::here('scripts', 'getLastfm.R'))
source(here::here('scripts', 'getSlugs.R'))

## Get Lastfm history
lastfm <- getLastfm(T) 

# Spotify auth ------------------------------------------------------------
spotID <- Sys.getenv('SPOTIFY_ID')
spotSecret <- Sys.getenv('SPOTIFY_SECRET')
spotAuth <- "https://accounts.spotify.com/authorize"

spotClient <- httr2::oauth_client(
    id = spotID,
    secret = spotSecret,
    token_url = "https://accounts.spotify.com/api/token",
    name = "last2spot"
)

getSpotAuth <- function (req) {
    req_oauth_auth_code(
        req,
        client = spotClient,
        auth_url = spotAuth, 
        scope = str_c('playlist-modify', 'playlist-modify-private',
                      'playlist-read-private', 'playlist-read-collaborative',
                      sep = " "),
        cache_disk = TRUE,
        redirect_uri = "http://localhost:1410/"
    )
}
# Get list of playlists ---------------------------------------------------
plReq <- httr2::request(base_url = "https://api.spotify.com/v1/users/tomcopple/playlists") %>% 
    getSpotAuth() %>% 
    req_perform()
plResp <- plReq %>% resp_body_json()
howManyPlaylists <- plResp %>% pluck('total')

allPlaylists <- data.frame()
reps <- (howManyPlaylists - 1) %/% 50 + 1

for (i in 1:reps) {
    getPlReq <- httr2::request(base_url = "https://api.spotify.com/v1/users/tomcopple/playlists") %>% 
        getSpotAuth() %>% 
        req_url_query(limit = 50, offset = 50 * (i - 1))
    getPlResp <- getPlReq %>% 
        req_perform() %>% 
        resp_body_string() %>% 
        jsonlite::fromJSON()
    getPlaylists <- getPlResp %>% 
        pluck('items') %>% 
        select_if(names(.) %in% c('name', 'id', 'images')) %>% 
        mutate(images = map_chr(images, function(x) {
            if(!is.null(x$url)) {
                # print(names(x))
                x %>% pull(url) %>% first()
            } else {
                ""
            }
        }))
    allPlaylists <- bind_rows(allPlaylists, getPlaylists) %>% 
        arrange(name)
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
totalReq <- httr2::request(base_url = str_glue("https://api.spotify.com/v1/playlists/{playlistID}/tracks")) %>% 
    getSpotAuth() %>% 
    req_url_query(fields = 'total')
totalResp <- httr2::req_perform(totalReq) %>% 
    resp_body_string() %>% 
    jsonlite::fromJSON()
totalTracks <- totalResp %>% pluck('total')

offsets <- seq.int(from = 0, to = floor((totalTracks-1)/100))

## Define function to get tracks
## x is offset, i.e. starts at 1 and goes up (defined by i)
getTracks <- function(x) {
    tracksReq <- httr2::request(base_url = str_glue("https://api.spotify.com/v1/playlists/{playlistID}/tracks")) %>% 
        getSpotAuth() %>% 
        req_url_query(fields = "items(track(artists.name,album(name),track_number,name, id))",
                      limit = 100, offset = x*100)
    tracksResp <- tracksReq %>% 
        req_perform() %>% 
        resp_body_string() %>% 
        jsonlite::fromJSON()
    
    tracksReturn <- tracksResp %>% 
        pluck('items','track') %>%
        as_tibble() %>% 
        mutate(album = purrr::flatten_chr(album),
               artist = purrr::map_chr(artists, ~first(unlist(.))), .keep = 'unused') %>% 
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
    mutate(track = case_when(
        artist == 'Omar Apollo' & str_detect(track, 'Evergreen') ~ 'Evergreen',
        TRUE ~ track
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
    # slice_sample(n = 10) %>% 
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
    arrange(n)
minPlays

## 2. top10 ----
top10 <- trackCount %>% 
    top_n(10) %>% 
    ## can't quite figure out best way of doing this
    ## Might have more than ten, but always want the most played anyway
    ## Just take random sample for now, even if you miss the max n
    slice_sample(n = 10) %>% 
    arrange(artist)
top10    

## Then try to send them all to the playlist
minReq <- httr2::request(base_url = str_c('https://api.spotify.com/v1/playlists/', "1BBr03knQFBNoj3EUN2rpm", '/tracks')) %>% 
    getSpotAuth() %>% 
    req_body_json(list(
        uris = str_glue("spotify:track:{minPlays$id}")
    )) %>% 
    req_method('PUT')
minResp <- req_perform(minReq)

topReq <- httr2::request(base_url = str_c('https://api.spotify.com/v1/playlists/', "1Th3m2O6NYvmx1rn5ymsYu", '/tracks')) %>% 
    getSpotAuth() %>% 
    req_body_json(list(
        uris = str_glue("spotify:track:{top10$id}")
    )) %>% 
    req_method('PUT')
topResp <- req_perform(topReq)



