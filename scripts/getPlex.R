## Get all Plex info

getPlex <- function(refresh = FALSE) {
    
    library(tidyverse);library(httr);library(lubridate);library(jsonlite);library(httr2)
    
    dropboxClient <- oauth_client(
        id = Sys.getenv('DROPBOX_KEY'),
        secret = Sys.getenv('DROPBOX_SECRET'),
        token_url = "https://api.dropboxapi.com/oauth2/token",
        name = 'Rstudio_TC'
    )
    dropboxToken <- readRDS('dropbox.RDS')
    
    if (refresh) {
        token <- "YTbYV3s5vkVVco6stFDW"
        plexRaw <- content(GET("http://192.168.1.99:32400/library/sections/3/search?type=10", 
                                 add_headers("X-Plex-Token" = token)), type = 'text') %>% 
            jsonlite::fromJSON() %>% 
            pluck('MediaContainer') %>% 
            pluck('Metadata')
        
        plex <- plexRaw %>% select(
                albumArtist    = grandparentTitle, 
                   artist      = originalTitle, 
                   album       = parentTitle, 
                   track       = title, 
                   rating      = userRating,
                   key         = ratingKey,
                   artistKey   = grandparentRatingKey,
                   albumKey    = parentRatingKey,
                trackNum       = index,
                discNum        = parentIndex,
                year           = parentYear,
                duration       = duration
            ) %>% 
            mutate(artist = ifelse(is.na(artist), albumArtist, artist)) %>% 
            as_tibble()
        
        write.csv(plex, file = here::here('tempData', 'plexDB.csv'), 
                  row.names = FALSE, fileEncoding = "UTF-8")
        
        reqUpload <- request('https://content.dropboxapi.com/2/files/upload/') %>% 
            req_oauth_refresh(client = dropboxClient, 
                              refresh_token = dropboxToken$refresh_token) %>% 
            req_headers('Content-Type' = 'application/octet-stream') %>% 
            req_headers(
                'Dropbox-API-Arg' = str_c('{',
                                          '"autorename":false,',
                                          '"mode":"overwrite",',
                                          '"path":"/R/lastfm/plexDB.csv",',
                                          '"strict_conflict":false', 
                                          '}')
            ) %>% 
            req_body_file(path = here::here('tempData', 'plexDB.csv'))
        
        respUpload <- req_perform(reqUpload)

    } else {
        
        reqDownload <-  request("https://content.dropboxapi.com/2/files/download") %>% 
            req_oauth_refresh(client = dropboxClient, 
                              refresh_token = dropboxToken$refresh_token) %>% 
            req_method('POST') %>%
            req_headers(
                'Dropbox-API-Arg' = str_c('{',
                                          '"path":"/R/lastfm/plexDB.csv"',
                                          '}')
            )
        
        respDownload <- req_perform(reqDownload,
                                    here::here('tempData', 'plexDB.csv'))
        
        plex <- readr::read_csv(here::here('tempData', 'plexDB.csv'), show_col_types = FALSE)
        
    }
    
    return(plex)
    
}
