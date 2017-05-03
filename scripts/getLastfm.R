# Download new lastfm scrobbles and save to local csv file
# Stored in Dropbox/R/lastfm/tracks.csv

# refresh = FALSE means just reading the csv. 
# Number of pages: 5 is ~ 1 month, so depends when it was last run. 
getLastfm <- function(refresh = FALSE, pages = 5) {
    
    library(tidyverse);library(lubridate);library(httr);library(jsonlite);
    basedir <- "~/Dropbox/R/lastfm"
    
    # Don't use readr as can't handle UTF-16, and some tracks/artists have foreign 
    # language encoding etc, just use base then convert to tibble.  
    localData <- read.csv(file.path(basedir, "tracks.csv"), stringsAsFactors = FALSE,
                          fileEncoding = "UTF-16LE") %>% 
        as_tibble()
    
    if(refresh == TRUE) {
        
        # Set user = username, api = api_key
        source("scripts/setLastfm.R")
        user <- setUser()
        api <- setAPI()
        
        # Set lastfm base url
        baseurl <- paste0(
            "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=", user,
            "&api_key=", api, "&format=json&limit=200&page="
        )
        
        # Initiate data.frame
        response <- data_frame()
        
        # Need to send multiple calls as limit of 200 per page. Roughly equal to 10 
        # per month, so depends on when it was last run. 
        for(i in 1:pages) {
            
            url <- paste0(baseurl, i)             # add page number to url
            
            ## Just print a basic progress message to console. 
            print(paste0(
                "This might take a couple of minutes, don't panic... ",
                100*(i/pages), "%"
                ))
            responseRaw <- httr::GET(url)           # GET data from url
            httr::stop_for_status(responseRaw)      # in case it fails. 
            
            # Get text from content and convert from json to dataframe
            responseClean <- responseRaw %>% 
                httr::content(., as = "text") %>% 
                jsonlite::fromJSON(., simplifyDataFrame = T, flatten = T) %>% 
                `[[`(c('recenttracks', 'track')) %>% 
                # Only keep track, title, album, date
                select(track = name, artist = `artist.#text`, 
                       album = `album.#text`, date = `date.#text`) %>% 
                # Need to keep time otherwise you might lose scrobbles from the day
                # you last ran the script. 
                mutate(date = lubridate::dmy_hm(date))
                
         
            # And rbind back to response
            response <- bind_rows(response, responseClean) %>% 
                arrange(desc(date)) %>% 
                na.omit()
            }
        
        # Then filter response for new tracks, and add to localData
        # Check to make sure that oldest value goes back far enough
        if(max(localData$date) > min(response$date)) {
            responseNew <- filter(response, date > max(localData$date))
            localData <- rbind(responseNew, localData)
            
            # Then write this back to csv
            # NB Don't use readr as can't handle other file encodings. 
            write.csv(x = localData, file = file.path(basedir, "tracks.csv"),
                      row.names = FALSE, fileEncoding = "UTF-16LE")
            
        } else print("Script didn't go back enough - run again with more pages")
        
        
        
    }
    
    lastfm <<- localData
}