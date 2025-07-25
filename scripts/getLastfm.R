# Download new lastfm scrobbles and save to local csv file
# Stored in Dropbox/R/lastfm/lastfmShiny/tracks.csv

# refresh = FALSE means just reading the csv.

getLastfm <- function(refresh = TRUE) {
    
    library(tidyverse);library(httr2);library(here)
    
    ## New system to refresh token - need to add &token_access_type=offline
    ## to the browser before authenticating token
    ## Also need to create the tempData folder
    
# Dropbox Auth ------------------------------------------------------------
    ## If this doesn't work, then
    # source('scripts/dropboxAuth.R')
    dropboxClient <- oauth_client(
        id = Sys.getenv('DROPBOX_KEY'),
        secret = Sys.getenv('DROPBOX_SECRET'),
        token_url = "https://api.dropboxapi.com/oauth2/token",
        name = 'Rstudio_TC'
    )
    dropboxToken <- readRDS(here::here('dropbox.RDS'))

# Dropbox Download --------------------------------------------------------

    reqDownload <- request("https://content.dropboxapi.com/2/files/download") %>% 
        req_oauth_refresh(client = dropboxClient, 
                          refresh_token = dropboxToken$refresh_token) %>% 
        req_method('POST') %>%
        req_headers(
            'Dropbox-API-Arg' = str_c('{',
                                        '"path":"/R/lastfm/tracks.csv"',
                                        '}')
        )
        
    respDownload <- req_perform(reqDownload,
                                path = here::here('tempData', 'tracks.csv'))
    
    localData <- readr::read_csv(here::here('tempData', 'tracks.csv'), lazy = FALSE) %>%
        mutate(date = lubridate::ymd_hms(date)) %>%
        filter(!is.na(date)) %>%
        as_tibble()
    

# Lastfm Refresh ----------------------------------------------------------

    
    if (refresh) {
        maxDate = max(localData$date)
        user <- Sys.getenv('LASTFM_USER')
        api <- Sys.getenv('LASTFM_APIKEY')
        
        responseDF <- tibble()
        pageNum <- 1
        
        # Keep getting data from lastfm until it's caught up with local data.
        while (min(responseDF$date) >= maxDate) {
            
            baseurl <- "http://ws.audioscrobbler.com/2.0/"
            reqLFM <- request(str_c(
                baseurl,
                "?method=user.getrecenttracks&user=", user, 
                "&api_key=", api, "&format=json&limit=200",
                "&page=", pageNum))
            
            respLFM <- req_perform(reqLFM) %>% 
                resp_body_json()
                
        respTidy <- respLFM %>% 
            pluck('recenttracks', 'track') %>% 
            map_df(function(x) {
                df <- tibble(
                    track = x$name,
                    artist = x$artist$`#text`,
                    album = x$album$`#text`,
                    date = x$date$uts
                )
                return(df)
            }) %>% 
            mutate(date = lubridate::as_datetime(as.numeric(date))) %>% 
            na.omit()
        
        responseDF <- bind_rows(responseDF, respTidy) %>%
                arrange(desc(date))
        pageNum <- pageNum + 1
        }
        
        # Then filter response for new tracks, and add to localData
        localData <- bind_rows(filter(responseDF, date > maxDate),
                               localData)
        
        ## Write a local csv (ignored in git) and upload to Dropbox. 
        readr::write_excel_csv(x = localData,
                               file = "tempData/tracks.csv")
        # write.csv(localData, file = "tempData/tracks.csv", row.names = FALSE, fileEncoding = "UTF-16LE")
        
        reqUpload <- request('https://content.dropboxapi.com/2/files/upload/') %>% 
            req_oauth_refresh(client = dropboxClient, 
                              refresh_token = dropboxToken$refresh_token) %>% 
            req_headers('Content-Type' = 'application/octet-stream') %>% 
            req_headers(
                'Dropbox-API-Arg' = str_c('{',
                                          '"autorename":false,',
                                          '"mode":"overwrite",',
                                          '"path":"/R/lastfm/tracks.csv",',
                                          '"strict_conflict":false', 
                                          '}')
                ) %>% 
            req_body_file(path = here::here('tempData', 'tracks.csv'))
        respUpload <- req_perform(reqUpload)
    }
    
    return(localData)
}
