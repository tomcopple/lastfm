## Get lastfm scrobbles for Shiny App
library(rdrop2);library(tidyverse)
getLastfmShiny <- function() {
    print("Getting Lastfm Shiny")
    
    # Change of plan - just use a csv of tracks in Dropbox and read/write from that
    # I'll just have to manually update the Dropbox file occasionally from home.
    dropbox <- readRDS('dropbox.rds')
    rdrop2::drop_download("R/lastfm/tracks.csv", 
                          local_path = "tracks-old.csv", overwrite = TRUE, dtoken = dropbox)
    localData <- readr::read_csv('tracks-old.csv') %>% 
        mutate(date = lubridate::ymd_hms(date)) %>% 
        filter(!is.na(date)) %>% 
        as_tibble()
    maxDate = max(localData$date)
    
    # Username and Api key are stored as environmental variables. (?)
    # Not sure how/if this works...
    user <- Sys.getenv('LASTFM_USER')
    print(user)
    api <- Sys.getenv('LASTFM_APIKEY')
    
    responseDF <- tibble()
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
    
    ## Then save updated data as a local csv and upload back to Dropbox. 
    write.csv(localData, file = "tracks.csv", row.names = FALSE, fileEncoding = "UTF-8")
    rdrop2::drop_upload(file = "tracks.csv", path = "R/lastfm", dtoken = dropbox)
    
    return(localData)
}
