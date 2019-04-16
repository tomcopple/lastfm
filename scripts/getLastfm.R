# Download new lastfm scrobbles and save to local csv file
# Stored in Dropbox/R/lastfm/lastfmShiny/tracks.csv

# refresh = FALSE means just reading the csv.

getLastfm <- function(refresh = TRUE) {
    library(tidyverse);library(rdrop2);library(here)
    if (refresh) {
        rdrop2::drop_download("R/lastfm/tracks.csv",
                          local_path = "tracks.csv",
                          overwrite = T)
    }
    
    
    localData <- readr::read_csv("tracks.csv") %>%
        mutate(date = lubridate::ymd_hms(date)) %>%
        filter(!is.na(date)) %>%
        as_tibble()
    maxDate = max(localData$date)
    user <- Sys.getenv('LASTFM_USER')
    api <- Sys.getenv('LASTFM_APIKEY')
    
    responseDF <- tibble()
    pageNum <- 1
    
    # Keep getting data from lastfm until it's caught up with local data.
    while (min(responseDF$date) >= maxDate) {
        url <- paste0(
            "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=",
            user,
            "&api_key=",
            api,
            "&format=json&limit=200&page=",
            pageNum
        )
        responseRaw <- url %>%
            httr::GET(.) %>%
            httr::content(., as = "text") %>%
            jsonlite::fromJSON(., simplifyDataFrame = T, flatten = T) %>%
            magrittr::extract2(c('recenttracks', 'track')) %>%
            select(
                track = name,
                artist = `artist.#text`,
                album = `album.#text`,
                date = `date.#text`
            ) %>%
            mutate(date = lubridate::dmy_hm(date)) %>%
            na.omit()
        responseDF <- bind_rows(responseDF, responseRaw) %>%
            arrange(desc(date))
        pageNum <- pageNum + 1
    }
    
    # Then filter response for new tracks, and add to localData
    localData <- bind_rows(filter(responseDF, date > maxDate),
                                  localData)
    
    ## Write a local csv (ignored in git) and upload to Dropbox. 
    write.csv(localData, file = "tracks.csv", row.names = FALSE, fileEncoding = "UTF-8")
    rdrop2::drop_upload(file = "tracks.csv", path = "R/lastfm")
    
    return(localData)
}
