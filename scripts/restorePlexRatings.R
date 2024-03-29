## Restore all ratings

library(tidyverse);library(httr)


# 1. Download and import restore file -------------------------------------
## Need to go to dropbox version history and download a previous version
## Save as restoreRatings.csv somewhere

restoreRatings <- read_csv(file.path('tempData', 'restoreRatings.csv'))

## Load Plex library and get key identifiers for rated tracks
token <- 'ABhPTJsJFC1CsCPKzzhb'

## Don't bother with artist, just keep album artist as easier?
allTracks <- content(GET("http://192.168.1.99:32400/library/sections/3/search?type=10", 
                         add_headers("X-Plex-Token" = token))) %>% 
    magrittr::extract2('MediaContainer') %>% 
    magrittr::extract2('Metadata') %>% 
    map_df(function(x) {
        as_tibble(x) %>% 
            select(any_of(c("grandparentTitle", "parentTitle", "title", "userRating", 'ratingKey', 'userRating')))
    }) %>% 
    select(albumArtist = grandparentTitle, album = parentTitle, track = title, ratingKey, userRating)

## Add ratingKey to ratings
## Only keep ones where rating != userRating
combined <- restoreRatings %>% 
    mutate(track = str_to_lower(track),
           albumArtist = str_to_lower(albumArtist),
           album = str_to_lower(album)) %>% 
    left_join(allTracks %>% 
                  mutate(track = str_to_lower(track),
                         albumArtist = str_to_lower(albumArtist),
                         album = str_to_lower(album)), 
              by = c('albumArtist', 'album', 'track')) %>% 
    mutate(userRating = replace_na(userRating, 0)) %>% 
    filter(rating != userRating)
    

## Check for mismatches
missed <- filter(combined, is.na(ratingKey))
missed %>% 
    count(album, sort = T)

## Final dataframe to send to Plex
finalDF <- filter(combined, !is.na(ratingKey))

## Initialise empty dataframe in case of problems
issues <- tibble(ratingKey = "", userRating = "")

## For everything else, try to send rating
sendRating <- function(ratingKey, userRating) {
    req <- httr::GET("http://192.168.1.99:32400/:/rate", 
              query = list(
                  "X-Plex-Token" = token,
                  "identifier"   = "com.plexapp.plugins.library",
                  "key"          = ratingKey,
                  "rating"       = as.character(userRating)))
    if(status_code(req) != 200) {
        print(str_c("Problem with ", ratingKey))
        .GlobalEnv$issues <- rbind(issues, tibble(
            ratingKey = ratingKey, 
            userRating = userRating
            ))
    } else if (status_code(req) == 200) {
        print(str_c(ratingKey, " successful"))
    }
    
}

purrr::walk2(.x = finalDF$ratingKey, .y = finalDF$rating, .f = sendRating)

