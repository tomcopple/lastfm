## Try to scrobble something?
library(tidyverse);library(httr);library(glue);library(openssl);library(xml2)


# 1a. Authentication (actual) ----------------------------------------------

# user <- Sys.getenv('LASTFM_USER')
# api <- Sys.getenv('LASTFM_APIKEY')


# 1b. Authentication (testing) --------------------------------------------
user <- Sys.getenv('LASTFM_TESTING_USER')
api <- Sys.getenv('LASTFM_TESTING_APIKEY')
secret <- Sys.getenv('LASTFM_TESTING_SECRET')
finalKey <- Sys.getenv('LASTFM_TESTING_SESSION')

## Renew authentication (if it's not working) ----
## Get initial token
authURL <- glue::glue("http://ws.audioscrobbler.com/2.0/?method=auth.gettoken&api_key={api}&format=json")
token <- httr::GET(authURL) %>% content() %>% pluck('token')

## Authenticate in browser
httr::BROWSE(url = glue::glue("http://www.last.fm/api/auth/?api_key={api}&token={token}"))

## Get session token
api_sig <- md5(glue::glue("api_key{api}methodauth.getSessiontoken{token}{secret}"))
sessionURL <- glue::glue("http://ws.audioscrobbler.com/2.0/?method=auth.getSession&api_key={api}&token={token}&api_sig={api_sig}")
session <- httr::GET(sessionURL)
finalKey <- content(session) %>% 
    xml2::xml_find_all('//key') %>% 
    xml_text()

# 2. Scrobble -------------------------------------------------------------

timeNow <- as.numeric(lubridate::now())

album <- "Unknown Album"
albumArtist <- "Artist"
artist <- "Artist"
track <- "TrackName"

scrobbles <- list(
    album = album,
    api_key = api,
    artist = artist,
    method = 'track.scrobble',
    sk = finalKey,
    timestamp = timeNow,
    track = track
)

scrobbles <- scrobbles[order(names(scrobbles))]

scrobblesVec <- str_flatten(c(
    imap_chr(scrobbles, function(x, y) {
        str_c(y, x)
        }),
    secret))

scrobblesHash <- openssl::md5(scrobblesVec)

scrobblesSigned <- scrobbles
scrobblesSigned$api_sig <- scrobblesHash

scrobblesURL <- httr::POST("http://ws.audioscrobbler.com/2.0/",
                          body = scrobblesSigned, encode = 'form')
res <- content(scrobblesURL)
res2 <- as_list(res)
res3 <- res2$lfm$scrobbles$scrobble
res3

# 3. Try as function ----

## Scrobble from Plex?
source(here::here("R", "bootstrap.R"))
source_project("R", "lib", "getPlex.R")
plex <- getPlex(F)

findArtist <- "Four Tet"
findAlbumName <- "Morning"


findTracks <- plex %>% 
        filter(str_detect(albumArtist, findArtist),
               str_detect(album, findAlbumName)) %>%
        arrange(trackNum) %>% 
        distinct(albumArtist, artist, album, track, duration)
    
findTracks

scrobbleAlbum <- function(scrobbles = findTracks, timeStart) {
    
    timeUnix <- as.numeric(lubridate::ymd_hm(timeStart))
    
    scrobbles <- mutate(scrobbles, 
                        totalDuration = cumsum(duration),
                        timestamp = timeUnix + totalDuration/1000) %>% 
        select(albumArtist, artist, album, track, timestamp)
    
    pmap(scrobbles, function(artist, albumArtist, album, track, timestamp) {
        scrobbleTrack <- list(
            album = album,
            api_key = api,
            artist = artist,
            method = 'track.scrobble',
            sk = finalKey,
            timestamp = timestamp,
            track = track
        )
        
        scrobblesVec <- str_flatten(
            c(
                imap_chr(scrobbleTrack, function(x, y) {
                    str_c(y, x)
                }),
                secret))    
        
        scrobblesHash <- openssl::md5(scrobblesVec)
        scrobblesSigned <- scrobbleTrack
        scrobblesSigned$api_sig <- scrobblesHash
        
        scrobblesURL <- httr::POST("http://ws.audioscrobbler.com/2.0/",
                                   body = scrobblesSigned, encode = 'form')
        res <- content(scrobblesURL)
        res2 <- as_list(res)
        res3 <- res2$lfm$scrobbles$scrobble
        res3
        
    })

    
    
    
}
scrobbleAlbum(timeStart = "2025-08-26 14:00")
