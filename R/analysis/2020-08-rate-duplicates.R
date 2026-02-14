## Speed up ratings by identifyin duplicates and rating both?

source(here::here("R", "bootstrap.R"))
source_project("R", "lib", "getPlex.R")
source_project("R", "lib", "getLastfm.R")

plex <- getPlex(T) %>% 
    mutate(artist = ifelse(is.na(artist), albumArtist, artist))


## First check for any tracks with different ratings
plex %>% 
    group_by(artist, track) %>% 
    filter(n() > 1) %>% 
    mutate(newRat = mean(rating, na.rm = TRUE)) %>% 
    filter(!is.na(newRat)) %>% 
    select(artist, albumArtist, album, track, rating, key, newRat) %>% 
    arrange(artist) %>% 
    filter(rating != newRat)

## Get duplicates where one is rated and one isn't
dups <- plex %>% 
    group_by(artist, track) %>% 
    filter(n() > 1) %>% 
    mutate(newRat = mean(rating, na.rm = TRUE)) %>% 
    filter(!is.na(newRat)) %>% 
    filter(is.na(rating)) %>% 
    select(artist, albumArtist, album, track, rating, key, newRat) %>% 
    arrange(artist)

token <- '2CdDkLKF5xY27xxuxHB5'

## Copy this from restorePlexRatings.R

issues <- tibble(ratingKey = "", userRating = "")
sendRating <- function(ratingKey, userRating) {
    req <- httr::GET("http://192.168.1.99:32400/:/rate", 
                     query = list(
                         "X-Plex-Token" = token,
                         "identifier"   = "com.plexapp.plugins.library",
                         "key"          = ratingKey,
                         "rating"       = as.character(userRating)))
    if(status_code(req) != 200) {
        print(str_c("Problem with ", ratingKey))
        .GlobalEnv$issues <- rbind(issues, ratingKey = ratingKey, userRating = userRating)
    } else if (status_code(req) == 200) {
        print(str_c(ratingKey, " successful"))
    }
    
}

purrr::walk2(.x = dups$key, .y = dups$newRat, .f = sendRating)
