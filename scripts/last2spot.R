## Get Playlist of tracks with more than 5 plays but not in the last year
library(tidyverse);library(httr);library(purrr);library(stringr);library(spotifyr)
library(here);library(lubridate)
source(here::here('scripts', 'getLastfm.R'))

lastfm <- getLastfm(T)

## Filter for playcount > 5, not played in last year
getPlaylist <- function(playCount = 6, lastPlayed = 5, howMany = 25) {
    
    lastfm <- lastfm %>% 
        ## NA albums get removed, so convert to character
        mutate(album = ifelse(is.na(album), 'NA', album)) %>% 
        mutate_if(is.character, str_to_title) %>% 
        ##Testing this out - remove everything after a ( or a - in a 
        ## track title, to get rid of (2019 remaster), (demo), (alternate) etc
        mutate(track = str_remove_all(track, "\\s[\\(\\-\\[].*")) %>% 
        filter(str_detect(artist, "Stars Of The Lid", negate = TRUE),
               str_detect(artist, "Eluvium", negate = TRUE),
               str_detect(artist, "Winged Victory", negate = TRUE),
               str_detect(artist, "Inventions", negate = TRUE),
               str_detect(artist, "Brian Mcbride", negate = TRUE),
               str_detect(artist, 'Symphony|Orchestra', negate = TRUE),
               str_detect(artist, "Beethoven|Mozart|Chopin", negate = TRUE),
               str_detect(album, 'Beethoven|Mozart|Chopin', negate = TRUE))
    
    ## Get list of potential tracks, remove unwanted Artists
    potentialTracks <- lastfm %>% 
        group_by(artist, track) %>% 
        summarise(n = n(), date = max(date)) %>% 
        ungroup() %>% 
        dplyr::filter(n > playCount, 
               date < lubridate::today() - lubridate::years(lastPlayed)) %>% 
        arrange(desc(n))
    
    print(str_c("Potential tracks: ", nrow(potentialTracks)))
    if (howMany > nrow(potentialTracks)) {
        howMany <- nrow(potentialTracks)
    }
    
    ## Save playlist here
    playlist <- sample_n(potentialTracks, howMany) %>% 
        unite(col = tracks, artist, track, sep = " ") %>% 
        select(-n, -date) %>% 
        magrittr::extract2('tracks')
    
    ## Plot chart of monthly plays for all tracks, for fun
    
    chart <- lastfm %>%
        mutate_if(is.character, str_to_title) %>% 
        unite(col = tracks, artist, track, sep = " ") %>% 
        filter(tracks %in% playlist) %>% 
        mutate(month = floor_date(date, 'month')) %>% 
        group_by(tracks, month) %>% count() %>% ungroup() %>% 
        mutate(tracks = forcats::fct_rev(tracks)) %>%  
        ggplot(aes(x = month, y = tracks, color = tracks, size = n)) + 
        geom_point() + 
        theme(legend.position = 'none') + xlab("") + ylab("")
    print(chart)
    
    
    ## Set up Spotify authentication
    playlistID <- "1BBr03knQFBNoj3EUN2rpm"
    
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
    spotIDs <- playlist %>% map_chr(., getIDs)
    

    
    ## Then try to send them all to the playlist
    httr::PUT(url = str_c('https://api.spotify.com/v1/playlists/', playlistID, '/tracks'),
              body = list(uris = str_c('spotify:track:', na.omit(spotIDs))),
              httr::config(token = spotAuth), encode = 'json'
    )
    
    print(enframe(playlist))
    return(enframe(playlist))
    
     
    }
playlist <- getPlaylist(6, 5, 25)




