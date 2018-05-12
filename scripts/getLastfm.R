# Download new lastfm scrobbles and save to local csv file
# Stored in Dropbox/R/lastfm/tracks.csv

# refresh = FALSE means just reading the csv. 
# Number of pages: 5 is ~ 1 month, so depends when it was last run. 
getLastfm <- function(refresh = FALSE) {
    
    library(tidyverse);library(lubridate);library(httr);library(jsonlite);
    basedir <- "~/Dropbox/R/lastfm"
    
    # Don't use readr as can't handle UTF-16, and some tracks/artists have foreign 
    # language encoding etc, just use base then convert to tibble.  
    localData <- read.csv(file.path(basedir, "tracks.csv"), stringsAsFactors = FALSE,
                          fileEncoding = "UTF-16LE") %>% 
        mutate(date = lubridate::ymd_hms(date)) %>% 
        as_tibble()
    
    if(refresh) {
        
        # Set user = username, api = api_key
        source(file.path("scripts", "setLastfm.R"))
        user <- setUser()
        api <- setAPI()
        
        # Set lastfm base url
        baseurl <- paste0(
            "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=", user,
            "&api_key=", api, "&format=json&limit=200&page="
        )
        
        # Initiate data.frame
        responseDF <- tibble()
        pageNum <- 1
        
        # Get time/date of most recent local scrobble
        maxDate <- max(localData$date)
        
        # Keep getting data from lastfm until it's caught up with local data
        while(min(responseDF$date) >= maxDate) {
            url <- stringr::str_c(baseurl, pageNum, sep = "")
            
            responseRaw <- url %>%
                httr::GET(.) %>% 
                httr::content(., as = "text") %>% 
                jsonlite::fromJSON(., simplifyDataFrame = T, flatten = T) %>% 
                magrittr::extract2(c("recenttracks", "track")) %>% 
                select(track = name, artist = `artist.#text`,
                       album = `album.#text`, date = `date.#text`) %>% 
                mutate(date = lubridate::dmy_hm(date)) %>% 
                na.omit()
            responseDF <- bind_rows(responseDF, responseRaw) %>% 
                arrange(desc(date))
            
            pageNum <- pageNum + 1
        }
        
        localData <- bind_rows(
            filter(responseDF, date > maxDate),
            localData
        )
        
        write.csv(localData, file = file.path(basedir, "tracks.csv"),
                      row.names = FALSE, fileEncoding = "UTF-16LE")
        
        
    }
    
    .GlobalEnv$lastfm <- localData
}
