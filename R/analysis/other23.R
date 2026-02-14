## Other 2023 albums, i.e. not in new music or 2023 albums

library(tidyverse);library(lubridate);library(httr);library(httr2);library(jsonlite)
source(here::here("R", "bootstrap.R"))
source_project("R", "lib", "getLastfm.R")
source_project("R", "lib", "getSlugs.R")

tracks <- getLastfm(T)

new23 <- tracks %>% 
    mutate(year = year(date)) %>%
    mutate(across(where(is.character), str_to_title)) %>% 
    group_by(artist, album) %>%
    count(track, year, name = 'trackPlays') %>%
    group_by(track) %>%
    top_n(n = -1, wt = year) %>%
    group_by(artist, album) %>% add_count(year, name = 'newTracks') %>%
    filter(year == 2023, newTracks > 5) %>% 
    group_by(artist, album, newTracks) %>% 
    tally(trackPlays, name = 'albumPlays') %>% 
    ungroup() %>% 
    arrange(desc(albumPlays)) %>% na.omit()

## Make some manual adjustments, and remove anything not wanted
allNew23 <- new23 %>% 
    filter(str_detect(album, 'Everything Everywhere All', negate = T),
           album != 'The Low End Theory',
           str_detect(album, 'Ethiopiques', negate = T),
           album != 'Heavy Elevator',
           str_detect(album, 'The Beginning Stages', negate = T),
           album != 'Uprooted',
           album != 'One Year',
           album != 'Sleep',
           album != 'Mia Gargaret',
           str_detect(album, 'Bait Ones', negate = T),
           str_detect(album, 'Sophtware', negate = T),
           artist != 'Eluvium',
           artist != 'Penguin Cafe',
           str_detect(artist, '[Bb]oygenius', negate = T),
           str_detect(artist, 'Yoh', negate = T),
           artist != 'boygenius',
           str_detect(album, 'Complete Works', negate = T),
           artist != 'Otis Redding',
           str_detect(artist, 'Kaytram', negate = T)) 

## Download New Music and 2023 albums to filter out
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

getThem <- allPlaylists %>% 
    filter(name %in% c('2023 Albums', 'New Music'))

## Can only get 100 tracks at a time, so need to know how many times to run it
totalTracks <- map_dfr(.x = getThem$id %>% purrr::set_names(),
                   .f = function(playlistID) {
                       res <- httr::GET(url = str_c('https://api.spotify.com/v1/playlists/', playlistID, '/tracks'),
                                 query = list(fields = "total"),
                                 httr::config(token = spotAuth), encode = 'json') %>% 
                           content(., as = 'text') %>% fromJSON() %>% pluck('total')
                       offsets <- seq.int(from = 0, to = floor((res-1)/100))
                       tibble(n = res, offsets = offsets)
                   }, .id = 'id')
    

## Define function to get tracks
## x is offset, i.e. starts at 1 and goes up (defined by i)

allExisting <- map2(.x = totalTracks$id, 
                    .y = totalTracks$offsets,
                    .f = function(x, y) {
                        tracksRaw <- httr::GET(url = str_glue("https://api.spotify.com/v1/playlists/{x}/tracks"),
                                               query = list(
                                                   fields = "items(track(artists.name, album(name), name, id))",
                                                   limit = 100, offset = y * 100),
                                               httr::config(token = spotAuth), encode = 'json'
                                               )
                        tracksReturn <- content(tracksRaw, as = 'text') %>% 
                            fromJSON() %>% pluck('items') %>% pluck('track') %>% 
                            as_tibble() %>% 
                            mutate(album = purrr::flatten_chr(album),
                                   artist = purrr::map_chr(artists, function(x) first(unlist(x))), .keep = 'unused') %>% 
                            rename(track = name)
                        return(tracksReturn)
                    }) %>% 
    purrr::list_rbind() %>% 
    distinct(artist, album)

## Check they're all in new23
allExisting %>% 
    mutate(across(where(is.character), getSlugs)) %>% 
    anti_join(allNew23 %>% mutate(across(where(is.character), getSlugs)))

## then do the opposite to get albums not in New Music or 2023 Albums
other23 <- allNew23 %>% 
    mutate(across(where(is.character), getSlugs)) %>% 
    anti_join(allExisting %>% mutate(across(where(is.character), getSlugs))) %>% 
    unite(artist, album, sep = " - ", col = 'albumQuery')
    

## Then need to find these albums on spotify to add to playlist
## Also check that they were released in the last couple of years to avoid mismatches

getIDs <- function(album) {
    query <- list(q = album, type = 'album', market = 'GB', limit = 1)
    res <- httr::GET(url = "https://api.spotify.com/v1/search", 
                     query = query, 
                     httr::config(token = spotAuth), encode = 'json')
    resContent <- content(res)
    if (resContent$albums$total > 0) {
        print(resContent$albums$items[[1]]$name)
        resContent <- resContent %>% 
            purrr::keep(str_sub(.$albums$items[[1]]$release_date, 0, 4) %in% c('2022', '2023'))
        if (length(resContent) > 0) {
                    resID <- resContent$albums$items[[1]]$id

        } else {
            resID <- NA
        }
    } else {
        resID <- NA
    }
    return(resID)
}

other23ids <- map_chr(other23$albumQuery, getIDs)

## Then need to get track ids

trackIDs <- map(other23ids, function(x) {
    tracksRaw <- httr::GET(url = str_glue("https://api.spotify.com/v1/albums/{x}/tracks"),
                           query = list(
                               fields = "items(id))",
                               limit = 49),
                           httr::config(token = spotAuth), encode = 'json'
    )
    tracksReturn <- content(tracksRaw, as = 'text') %>% 
        fromJSON() %>% pluck('items') %>% pluck('id') 
    return(tracksReturn)
}) %>% unlist()


## Then add to playlist, can only do 100 at a time
other23playlist <- "3ApZgfOFkhqba1zI0iwQzO"

nPuts <- length(trackIDs) %/% 100 + 1

for (i in 1:nPuts) {
    nTracks <- trackIDs[c(((i - 1) * 100 + 1): (i * 100)) ]
    print(length(nTracks))
    if (i == 1) {
        httr::PUT(url = str_glue('https://api.spotify.com/v1/playlists/{other23playlist}/tracks'),
                   httr::config(token = spotAuth), encode = 'json',
                   body = list(uris = str_c("spotify:track:", nTracks))
        )
    } else {
        httr::POST(url = str_glue('https://api.spotify.com/v1/playlists/{other23playlist}/tracks'),
                   httr::config(token = spotAuth), encode = 'json',
                   body = list(uris = str_c("spotify:track:", nTracks))
        )        
    }

}

