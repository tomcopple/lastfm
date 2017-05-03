# Backup script if you need to restore all scrobbles for some reason

restoreLastfm <- function(env = .GlobalEnv) {
    library(tidyverse);library(lubridate);library(httr);library(jsonlite)
    library(readxl)
    
    basedir <- "~/Dropbox/R/lastfm"

    # Set user = username, api = api_key
    source("setLastfm.R")
    user <- setUser()
    api <- setAPI()
    
    # Set lastfm base url
    baseurl <- paste0(
        "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=", user,
        "&api_key=", api, "&format=json&limit=200&page="
    )
    
    # Initiate data frame
    restore <- data.frame()
    
    # Get 200 per page and need ~120,000 responses, so ~600 pages in total?
    # Find the highest page?
    pages <- (nrow(lastfm) %/% 200) + 1
    
    for(i in pages:1) {
        
        url <- paste0(baseurl, i)
        
        print(paste0(
            "This might take a couple of minutes, don't panic... [",
            i, "]"
        ))  
        
        # Send httr GET request and format response
        restore1 <- httr::GET(url)
        httr::stop_for_status((restore1))
        restore2 <- httr::content(restore1, as = "text")
        restore3 <- fromJSON(restore2, simplifyDataFrame = T, flatten = T)
        restore4 <- restore3$recenttracks
        restore5 <- restore4$track
        
        restore6 <- select(restore5,
                           track = name, artist = `artist.#text`, album = `album.#text`) %>% 
            mutate(date = parse_date_time(restore5$`date.#text`, "d b Y, H:M")) %>% 
            filter(!is.na(date)) %>% 
            as_data_frame()
        
        # Rbind back to response
        restore <- rbind(restore, restore6) %>% 
            arrange(desc(date)) %>% 
            as_data_frame()
    }
    
write.csv(restore, file = file.path(basedir, "restoreLastfm.csv"), 
          row.names = FALSE, fileEncoding = "UTF-16LE")
    
}