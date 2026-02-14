## Orphans and misfits
library(tidyverse);library(httr2);library(jsonlite)
source(here::here("R", "bootstrap.R"))
source_project("R", "lib", "getLastfm.R")
tracks <- getLastfm(T)

selectYear <- 2015

playlist <- tracks %>% 
    na.omit() %>% 
    mutate_if(is.character, str_to_title) %>% 
    mutate(year = year(date)) %>% 
    filter(year == selectYear) %>% 
    add_count(artist,track, album, year) %>% 
    add_count(artist, album) %>% 
    # filter(nn < n)
    filter(n > 5, n >= nn * 0.5) %>% 
    filter(str_detect(album, 'Pitchfork', negate = T),
           str_detect(album, 'Stones Throw And', negate = T),
           str_detect(album, 'Dj-Kicks', negate = T),
           artist != 'Super Simple Songs',
           artist != 'Pinkfong',
           artist != 'Dwayne Johnson',
           artist != 'Roger Miller',
           artist != 'Raffi',
           artist != 'Melody Shakers',
           str_detect(track, 'Five Little Ducks', negate = T),
           str_detect(track, 'Taking A Bath', negate = T),
           str_detect(track, 'Bubble Bath In The Sink', negate = T),
           artist != 'Dora The Explorer',
           artist != 'Johann Sebastian Bach',
           str_detect(artist, 'The Very Best Feat', negate = T),
           str_detect(artist, 'The Very Best & ', negate = T),
           artist != 'Bluey') %>% 
    distinct(artist,track,album,n,nn) %>% 
    arrange(desc(n)) %>% 
    select(-album) %>% 
    unite(col = tracks, artist, track, sep = " ") %>%
    pull(tracks)


## Get Spotify playlists first, don't want to use songs already in the A-sides
## playlist
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
        redirect_uri = "http://localhost:1410/",
        cache_disk = TRUE
    )
}

## Check credentials are working
# spotToken <- httr2::oauth_flow_auth_code(
#     client = spotClient,
#     auth_url = spotAuth, 
#     redirect_uri = "http://localhost:1410/", 
#    scope = str_c('playlist-modify', 'playlist-modify-private',
#                 'playlist-read-private', 'playlist-read-collaborative',
#                sep = " ")
# )

# saveRDS(spotToken, "spotifyToken.RDS")


## Should be able to just import Spotify token on this computer
# spotToken <- readRDS(here::here('spotifyToken.RDS'))

# Get list of Spotify playlists ----
plReq <- httr2::request(base_url = "https://api.spotify.com/v1/users/tomcopple/playlists") %>% 
    getSpotAuth() %>% 
    req_perform()

plResp <- plReq %>% resp_body_json()
howManyPlaylists <- plResp %>% pluck('total')

allPlaylists <- data.frame()
reps <- howManyPlaylists %/% 50 + 1

for (i in 1:reps) {
    getPlReq <- httr2::request(base_url = "https://api.spotify.com/v1/users/tomcopple/playlists") %>% 
        getSpotAuth() %>% 
        req_url_query(limit = 50, offset = 50 * (i - 1))
    getPlResp <- getPlReq %>% 
        req_perform() %>% 
        resp_body_string() %>% 
        jsonlite::fromJSON()
    
    getPlaylists <- getPlResp %>% pluck('items') %>% select(name,id)
    
    allPlaylists <- bind_rows(allPlaylists, getPlaylists)
}

orphansID <- filter(allPlaylists, name == 'Orphans 2024') %>% pull(id)
orphansOther <- filter(allPlaylists, name == 'Orphans Other') %>% pull(id)
orphans24 <- filter(allPlaylists, name == 'Orphans 2024') %>% pull(id)
asides <- filter(allPlaylists, name == 'A sides') %>% pull(id)

# Download A sides playlist tracks ----
aReq <- httr2::request(
    base_url = str_glue("https://api.spotify.com/v1/playlists/{asides}/tracks")) %>% 
    getSpotAuth() %>% 
    req_perform()
aResp <- aReq %>% resp_body_string() %>% jsonlite::fromJSON() %>% 
    pluck('items', 'track') %>% 
    select(track = name, artists) %>% 
    unnest(cols = c(artists)) %>% 
    group_by(track) %>% filter(row_number() == 1) %>% 
    select(track, artist = name) %>% 
    unite(col = tracks, artist, track, sep = " ") %>% 
    mutate(tracks = janitor::make_clean_names(tracks))

# Create a master list of tracks to avoid ----
avoidTracks <- tracks %>% 
    distinct(artist, album, track) %>% 
    mutate_if(is.character, str_to_title) %>%
    na.omit() %>% 
    rowwise() %>% 
    filter(any(
        str_detect(album, 'Pitchfork'),
        str_detect(artist, 'Beethoven'),
        str_detect(album, 'Stones Throw And'),
        str_detect(album, 'Erased Tapes'),
        str_detect(album, 'Dj-Kicks'),
        artist == 'Super Simple Songs',
        artist == 'Pinkfong',
        artist == 'Dwayne Johnson',
        artist == 'Roger Miller',
        artist == 'Raffi',
        artist == 'Melody Shakers',
        str_detect(artist, 'Kiboomers'),
        str_detect(track, 'Five Little Ducks'),
        str_detect(track, 'Taking A Bath'),
        str_detect(track, 'Bubble Bath In The Sink'),
        artist == 'Dora The Explorer',
        artist == 'Johann Sebastian Bach',
        str_detect(artist, 'The Very Best Feat'),
        str_detect(artist, 'The Very Best & ')
    )) %>% 
    unite(col = tracks, artist, track, sep = " ", remove = F) %>%
    mutate(tracks = janitor::make_clean_names(tracks))

# Get playlist of Orphans ----
## Current year 2024 ----
playlist24 <- tracks %>% 
    # select(artist, track, date) %>% 
    na.omit() %>% 
    mutate_if(is.character, str_to_title) %>%
    mutate(year = year(date)) %>% 
    filter(year == 2024) %>% 
    add_count(artist,track, album, year) %>% 
    add_count(artist, album) %>% 
    # filter(nn < n)
    ## Get tracks played more than 6 times, and more than the rest of the 
    ## album put together
    filter(n > 6, n >= 0.5*nn) %>% 
    distinct(artist,track,album) %>% 
    select(-album) %>% 
    unite(col = tracks, artist, track, sep = " ") %>%
    mutate(tracksCheck = janitor::make_clean_names(tracks)) %>% 
    filter(!tracksCheck %in% avoidTracks$tracks,
           !tracksCheck %in% aResp$tracks) %>% 
    pull(tracks)


## Get all previous Orphans ----
playlistOther <- tracks %>% 
    # select(artist, track, date) %>% 
    na.omit() %>% 
    mutate_if(is.character, str_to_title) %>%
    mutate(year = year(date)) %>% 
    filter(year != 2024) %>% 
    add_count(artist,track, album, year) %>% 
    add_count(artist, album) %>% 
    # filter(nn < n)
    ## Get tracks played more than 6 times, and more than the rest of the 
    ## album put together
    filter(n > 6, n >= 0.5*nn) %>% 
    distinct(artist,track,album) %>% 
    select(-album) %>% 
    unite(col = tracks, artist, track, sep = " ") %>%
    mutate(tracksCheck = janitor::make_clean_names(tracks)) %>% 
    filter(!tracksCheck %in% avoidTracks$tracks,
           !tracksCheck %in% aResp$tracks) %>% 
    pull(tracks)

# Download song IDs from Spotify ----
getIDs <- function(track) {
    
    req <- httr2::request(base_url = "https://api.spotify.com/v1/search") %>% 
        getSpotAuth() %>% 
        req_url_query(
            q = track,
            type = 'track',
            market = 'GB',
            limit = 1
        )
    
    resp <- req_perform(req) %>% resp_body_json()
    
    if (resp$tracks$total > 0) {
        id <- resp$tracks$items[[1]]$id
    } else {
        id <- NA
    }
    return(id)
}

spotIDs24 <- playlist24 %>% map_chr(., getIDs) %>% na.omit()
spotIDsOther <- playlistOther %>% map_chr(., getIDs) %>% na.omit()

# Add to Spotify ----
addToSpot <- function(playlist) {
    print(rlang::as_name(enquo(playlist)))
    plName <- rlang::as_name(enquo(playlist))
    ext <- str_sub(plName, 8)
    print(ext)
    plId <- get(str_glue("orphans{ext}"))
    print(plId)
    
    if (length(playlist) > 100) {
        howManyGoes <- length(playlist) %/% 100
        
        firstGo <- playlist[c(1:100)]
        
        sendPlReq <- httr2::request(
            base_url = str_glue("https://api.spotify.com/v1/playlists/{plId}/tracks")) %>%
            req_method('PUT') %>%
            req_body_json(list(uris = str_c("spotify:track:", firstGo))) %>%
            getSpotAuth()
        sendPlResp <- req_perform(sendPlReq)
        
        for (i in 1:howManyGoes) {
            if ( (i + 1) * 100 <= length(playlist)) {
                nextGo <- playlist[c( ((i * 100) + 1) : ((i + 1) * 100) )]
            } else {
                nextGo <- playlist[c( ((i * 100) + 1) : length(playlist) )]
            }
            
            sendPlReq <- httr2::request(
                base_url = str_glue("https://api.spotify.com/v1/playlists/{plId}/tracks")) %>%
                req_method('POST') %>%
                req_body_json(list(uris = str_c("spotify:track:", nextGo))) %>%
                getSpotAuth()
            sendPlResp <- req_perform(sendPlReq)
        }
        
    } 
    else {
        sendPlReq <- httr2::request(base_url = str_glue("https://api.spotify.com/v1/playlists/{plId}/tracks")) %>%
            req_method('PUT') %>%
            req_body_json(list(uris = str_c("spotify:track:", playlist))) %>%
            getSpotAuth()
        
        sendPlResp <- req_perform(sendPlReq)
        
    }
}

addToSpot(spotIDs24)
addToSpot(spotIDsOther)    


