## Get lastfm scrobbles for Shiny App
library(rdrop2);library(tidyverse)
getLastfmShiny <- function() {
    
    # Change of plan - just use a csv of tracks in Dropbox and read/write from that
    # I'll just have to manually update the Dropbox file occasionally from home.
    dropbox <- readRDS('dropbox.rds')
    localData <- rdrop2::drop_read_csv('R/lastfm/tracks.csv', stringsAsFactors = FALSE,
                                       fileEncoding = "UTF-16LE", 
                                       dtoken = dropbox) %>% 
        mutate(date = lubridate::ymd_hms(date)) %>% 
        as_data_frame()
    maxDate = max(localData$date)
    
    # Username and Api key are stored as environmental variables. (?)
    # Not sure how/if this works...
    user <- Sys.getenv('LASTFM_USER')
    api <- Sys.getenv('LASTFM_APIKEY')
    
    responseDF <- data_frame()
    pageNum <- 1
    
    # Keep getting data from lastfm until it's caught up with local data. 
    while(min(responseDF$date) >= maxDate) {
        url <- paste0(
            "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=", user,
            "&api_key=", api, "&format=json&limit=200&page=", pageNum
        )
        responseRaw <- url %>%
            httr::GET(.) %>% 
            httr::content(., as = "text") %>% 
            jsonlite::fromJSON(., simplifyDataFrame = T, flatten = T) %>% 
            magrittr::extract2(c('recenttracks', 'track')) %>% 
            select(track = name, artist = `artist.#text`,
                   album = `album.#text`, date = `date.#text`) %>% 
            mutate(date = lubridate::dmy_hm(date)) %>% 
            na.omit()
        responseDF <- bind_rows(responseDF, responseRaw) %>% 
            arrange(desc(date))
        pageNum <- pageNum + 1
    }
    
    # Then filter response for new tracks, and add to localData
    localData <- dplyr::bind_rows(
        dplyr::filter(responseDF, date > maxDate),
        localData
    )
    
    return(localData)
}