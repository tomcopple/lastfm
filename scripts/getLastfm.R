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
    token <- readRDS('token.RDS')

# Dropbox Download --------------------------------------------------------

    reqDownload <- request("https://content.dropboxapi.com/2/files/download") %>% 
        # req_oauth_auth_code(
        #     client, port = 43451,
        #     auth_url = "https://www.dropbox.com/oauth2/authorize?token_access_type=offline"
        # ) %>% 
        req_auth_bearer_token(token$access_token) %>%
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
        write.csv(localData, file = "tempData/tracks.csv", row.names = FALSE, fileEncoding = "UTF-8")
        
        reqUpload <- request('https://content.dropboxapi.com/2/files/upload/') %>% 
            req_auth_bearer_token(token$access_token) %>%
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
