## Orphans and misfits
library(tidyverse);library(httr2);library(jsonlite)
source('scripts/getLastfm.R')
tracks <- getLastfm(T)

playlist <- tracks %>% 
    filter(year(date) == 2024) %>% 
    add_count(artist,track) %>% 
    add_count(artist, album) %>% 
    # filter(nn < n)
    filter(n > 5, nn == n) %>% 
    distinct(artist,track,album) %>% 
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
        cache_disk = TRUE,
        redirect_uri = "http://localhost:1410/"
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
spotToken <- readRDS(here::here('spotifyToken.RDS'))

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

orphansID <- filter(allPlaylists, name == 'Orphans 2024') %>% pull(id)

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

sendPlReq <- httr2::request(base_url = str_glue("https://api.spotify.com/v1/playlists/{orphansID}/tracks")) %>% 
    req_method('PUT') %>% 
    req_body_json(list(uris = str_c("spotify:track:", spotIDs))) %>% 
    getSpotAuth()

sendPlResp <- req_perform(sendPlReq)
