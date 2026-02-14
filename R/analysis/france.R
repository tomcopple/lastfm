## French playlist
library(tidyverse);library(lubridate);library(stringr);library(httr);library(spotifyr)

fDates <- c(
    seq.Date(from = dmy("01-10-2019"), to = dmy("06-10-2019"), by = "days"),
    seq.Date(from = dmy("16-05-2018"), to = dmy("22-05-2018"), by = 'days'),
    seq.Date(from = dmy("25-05-2017"), to = dmy("31-05-2017"), by = 'days'),
    seq.Date(from = dmy("30-09-2016"), to = dmy("04-10-2016"), by = 'days'),
    seq.Date(from = dmy("19-05-2016"), to = dmy("25-05-2016"), by = 'days'),
    seq.Date(from = dmy("21-05-2015"), to = dmy("26-05-2015"), by = 'days'),
    seq.Date(from = dmy("22-05-2014"), to = dmy("27-05-2014"), by = 'days'),
    seq.Date(from = dmy("21-02-2014"), to = dmy("24-02-2014"), by = 'days'),
    seq.Date(from = dmy("13-05-2013"), to = dmy("15-05-2013"), by = 'days'),
    seq.Date(from = dmy("24-05-2012"), to = dmy("28-05-2012"), by = 'days'),
    seq.Date(from = dmy("19-05-2011"), to = dmy("23-05-2011"), by = 'days')
)

source(here::here("R", "bootstrap.R"))
source_project("R", "lib", "getLastfm.R")
tracks <- getLastfm(F)

tracks %>% mutate(date = as_date(date)) %>% 
    filter(date %in% fDates) %>% 
    count(artist, track, sort = T) 

fTracks <- tracks %>% 
    mutate(date = as_date(date)) %>% 
    filter(date %in% fDates)


fTracks %>% count(artist, sort = T)

## Any artists with lots of plays but no track repeats?
fTracks %>% group_by(artist, track) %>% 
    add_count(sort = T) %>% 
    filter(n < 3) %>% ungroup() %>% 
    group_by(artist) %>% 
    add_count(sort = T) %>% 
    distinct(artist, nn)


## Join together two things: (1) any track with 3+ plays and (2) any track with 2+ plays if the artist has 10+ plays

db1 <- fTracks %>% 
    count(artist, track) %>% 
    filter(n > 3)
db2 <- fTracks %>% 
    count(artist, track) %>% 
    filter(n %in% c(2, 3)) %>% 
    ungroup() %>% 
    select(-n) %>% 
    add_count(artist) %>% 
    filter(n > 10)
fDB <- bind_rows(db1, db2) %>% select(-n)


# Spotify -----------------------------------------------------------------

## Set up Spotify authentication
playlistID <- "4goCfueUlVABu1Dd4UO2Ca"

token <- spotifyr::get_spotify_access_token(
    client_id = "95bdfb9cb87841208145ede83a2dd878", client_secret = "2ada4a2188b7420d8833b2fe783e5c03"
)

endpoint <- oauth_endpoint(
    authorize = "https://accounts.spotify.com/authorize",
    access =    "https://accounts.spotify.com/api/token")

app <- oauth_app(appname = "last2spot", key = "95bdfb9cb87841208145ede83a2dd878",secret = "2ada4a2188b7420d8833b2fe783e5c03")

spotAuth <- oauth2.0_token(endpoint = endpoint, app = app,
                           scope = c('playlist-modify', 'playlist-modify-private'))


## Look up and save playlist IDs
getIDs <- function(track, i) {
    print(str_c(i, ": ", track))
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
fList <- fDB %>% 
    na.omit() %>% 
    unite(col = 'tracks', artist, track, sep = " ") %>% 
    pull(tracks)
spotIds1 <- fList[c(1:100)] %>%  imap_chr(., getIDs)
spotIds2 <-  fList[c(101:198)] %>% imap_chr(., getIDs)
spotIDs <- c(spotIds1, spotIds2)


## Then try to send them all to the playlist
httr::POST(url = str_c('https://api.spotify.com/v1/playlists/', playlistID, '/tracks'),
          body = list(uris = str_c('spotify:track:', na.omit(spotIds1))),
          httr::config(token = spotAuth), encode = 'json'
)
