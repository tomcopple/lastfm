# Download new lastfm scrobbles and save to local csv file
# Stored in data/tracks.csv

basedir <- "~/Dropbox/R/lastfm"

library(tidyverse);library(lubridate);library(httr);library(jsonlite)

getLastfm <- function(refresh = FALSE, pages = 5) {
    # If refresh = FALSE, just loads data from csv
    localData <- read_csv(file.path(basedir, "tracks.csv"))
    
    if(refresh == TRUE) {
        
        # Set user = username, api = api_key
        source("setLastfm.R")
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
            print(paste0(
                "This might take a couple of minutes, don't panic... [",
                pages + 1 - i, "]"
                ))
            response1 <- httr::GET(url)           # GET data from url
            httr::stop_for_status(response1)      # in case it fails. 
            response1 <- httr::content(response1, as = "text")
            
            # Convert from json?
            response1 <- fromJSON(response1, simplifyDataFrame = T, flatten = T)
            response1 <- response1$recenttracks
            response1 <- response1$track
            
            # Only keeping track, title, album, date
            # NB Date needs to include time, otherwise you lose scrobbles from the day
            # you last run it if they're afterwards...
            response1 <- select(response1,
                                track = name, artist = `artist.#text`, album = `album.#text`, 
                                date = `date.#text`)
            response1 <- mutate(response1, 
                                date = parse_date_time(date, "d b Y, H:M"))
            # NB not piping because it doesn't seem to recognise the date...
            
            # And rbind back to response
            response <- rbind(response, response1) %>% 
                arrange(desc(date))
        }
        
        # Then filter response for new tracks, and add to localData
        # Check to make sure that oldest value goes back far enough
        if(max(localData$date > min(response$date))) {
            responseNew <- filter(response, date > max(localData$date))
            localData <- rbind(responseNew, localData)
            
            # Then write this back to csv
            write_csv(localData, file.path(basedir, "tracks.csv"))
        } else print("Script didn't go back enough - run again with more pages")
        
        
        
    }
    lastfm <<- localData
}