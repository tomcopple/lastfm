## Restore all ratings

## Firstly, read in the desired csv
filesRaw <- rdrop2::drop_search('plexRatings')

files <- filesRaw %>% 
    magrittr::extract2('matches') %>% 
    map_df(extract2, 2) %>% 
    magrittr::extract2('name')
print(files)

chooseData <- '2020-07-13'


rdrop2::drop_download(
    path = str_c('R/lastfm/', chooseData, '-plexRatings.csv'),
    local_path = file.path('tempData', 'temp.csv'))

ratings <- read_csv(file.path('tempData', 'temp.csv'))

## Load Plex library and get key identifiers for rated tracks

token <- '2CdDkLKF5xY27xxuxHB5'

## Don't both with artist, just keep album artist as easier?
allTracks <- content(GET("http://192.168.1.99:32400/library/sections/5/search?type=10", 
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
combined <- ratings %>% 
    left_join(allTracks, by = c('albumArtist', 'album', 'track')) %>% 
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
        .GlobalEnv$issues <- rbind(issues, ratingKey = ratingKey, userRating = userRating)
    } else if (status_code(req) == 200) {
        print(str_c(ratingKey, " successful"))
    }
    
}

purrr::walk2(.x = finalDF$ratingKey, .y = finalDF$rating, .f = sendRating)

