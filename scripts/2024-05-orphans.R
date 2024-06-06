## Orphans and misfits
library(tidyverse);library(httr2);library(jsonlite)
source('scripts/getLastfm.R')
tracks <- getLastfm(F)

playlist <- tracks %>% 
    na.omit() %>% 
    mutate_if(is.character, str_to_title) %>% 
    mutate(year = year(date)) %>% 
    filter(year != 2024) %>% 
    add_count(artist,track, album, year) %>% 
    add_count(artist, album) %>% 
    # filter(nn < n)
    filter(n > 6, nn <= 1.5*n) %>% 
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
           str_detect(artist, 'The Very Best & ', negate = T)) %>% 
    distinct(artist,track,album) %>% 
    select(-album) %>% 
    unite(col = tracks, artist, track, sep = " ") %>%
    pull(tracks)

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

plReq <- httr2::request(base_url = "https://api.spotify.com/v1/users/tomcopple/playlists") %>% 
    getSpotAuth() %>% 
    # httr2::req_oauth_refresh(
        # client = spotClient,
        # refresh_token = spotToken$refresh_token
    # ) %>% 
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

orphansID <- filter(allPlaylists, name == 'Orphans Other') %>% pull(id)

## Try to find song IDs
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
spotIDs <- playlist %>% map_chr(., getIDs) %>% na.omit()

if (length(spotIDs) > 100) {
    howManyGoes <- length(spotIDs) %/% 100
    
    firstGo <- spotIDs[c(1:100)]
    
    sendPlReq <- httr2::request(base_url = str_glue("https://api.spotify.com/v1/playlists/{orphansID}/tracks")) %>% 
        req_method('PUT') %>% 
        req_body_json(list(uris = str_c("spotify:track:", firstGo))) %>% 
        getSpotAuth()
    sendPlResp <- req_perform(sendPlReq)
    
    for (i in 1:howManyGoes) {
        if ( (i + 1) * 100 <= length(spotIDs)) {
            nextGo <- spotIDs[c( ((i * 100) + 1) : ((i + 1) * 100) )]
        } else { 
            nextGo <- spotIDs[c( ((i * 100) + 1) : length(spotIDs) )]
        }
        
        sendPlReq <- httr2::request(base_url = str_glue("https://api.spotify.com/v1/playlists/{orphansID}/tracks")) %>% 
            req_method('POST') %>% 
            req_body_json(list(uris = str_c("spotify:track:", nextGo))) %>% 
            getSpotAuth()
        sendPlResp <- req_perform(sendPlReq)
    }
    
} else {
    sendPlReq <- httr2::request(base_url = str_glue("https://api.spotify.com/v1/playlists/{orphansID}/tracks")) %>% 
        req_method('PUT') %>% 
        req_body_json(list(uris = str_c("spotify:track:", spotIDs[c(101:192)]))) %>% 
        getSpotAuth()
    
    sendPlResp <- req_perform(sendPlReq)
    
} 
