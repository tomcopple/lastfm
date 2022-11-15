# Download new lastfm scrobbles and save to local csv file
# Stored in Dropbox/R/lastfm/lastfmShiny/tracks.csv

# refresh = FALSE means just reading the csv.

getLastfm <- function(refresh = TRUE, refresh_token = TRUE) {
    
    ## NB Only need to refresh token every few hours
    ## So can set to false and try if it's soon after the last one
    library(tidyverse);library(rdrop2);library(here)
    
    ## New system to refresh token - need to add &token_access_type=offline
    ## to the browser before authenticating token
    # token <- drop_auth(new_user = T)
    # saveRDS(token, file = 'token.RDS')
    
    print('reading token')
    
        token <- readRDS('token.RDS')
    
    token    
    token$refresh
    
    print('read token')
    rdrop2::drop_download("R/lastfm/tracks.csv",
                          local_path = "tempData/tracks.csv",
                          overwrite = T)

    localData <- readr::read_csv("tempData/tracks.csv", lazy = FALSE) %>%
        mutate(date = lubridate::ymd_hms(date)) %>%
        filter(!is.na(date)) %>%
        as_tibble()
    
    if (refresh) {
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
        write.csv(localData, file = "tempData/tracks.csv", row.names = FALSE, fileEncoding = "UTF-8")
        rdrop2::drop_upload(file = "tempData/tracks.csv", path = "R/lastfm")
    }
    
    return(localData)
}
