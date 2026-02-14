# Download new lastfm scrobbles and save to local csv file
# Stored in Dropbox/R/lastfm/lastfmShiny/tracks.csv

# refresh = FALSE means just reading the csv.

getLastfm <- function(refresh = TRUE) {
    
    library(tidyverse);library(httr2);library(here);library(dropboxr)
    
    print("Downloading lastfm data from dropbox")
    lastfmDownload <- dropboxr::download_dropbox_file("/R/lastfm/tracks.csv") %>% 
        mutate(date = lubridate::ymd_hms(date)) %>% 
        filter(!is.na(date))


    # Lastfm Refresh ----------------------------------------------------------
    if (refresh) {
        print("Updating with new data from lastfm")
        maxDate = max(lastfmDownload$date)
        user <- Sys.getenv('LASTFM_USER')
        api <- Sys.getenv('LASTFM_APIKEY')
        
        responseDF <- tibble()
        pageNum <- 1
        
        # Keep getting data from lastfm until it's caught up with local data.
        while (min(responseDF$date) >= maxDate) {
            
            # 1. Build the request
            req <- request("http://ws.audioscrobbler.com/2.0/") %>%
                req_url_query(
                    method = "user.getrecenttracks",
                    user = user,
                    api_key = api,
                    format = "json",
                    limit = 200,
                    page = pageNum
                )
            
            # 2. Perform request and process data
            responseRaw <- req %>%
                req_retry(max_tries = 3) %>% 
                req_perform() %>%
                resp_body_json(simplifyVector = TRUE, flatten = TRUE) %>%
                purrr::pluck("recenttracks", "track") %>% 
                select(
                    track = name, 
                    artist = `artist.#text`,
                    album = `album.#text`, 
                    date = `date.#text`
                ) %>%
                mutate(date = dmy_hm(date)) %>%
                na.omit()
            
            responseDF <- bind_rows(responseDF, responseRaw) %>% 
                arrange(desc(date))
            pageNum <- pageNum + 1
        }
        
        # Then filter response for new tracks, and add to localData
        lastfmUpload <- bind_rows(
            filter(responseDF,date > maxDate),
            lastfmDownload
        )
        
        ## Then save updated data as a local csv and upload back to Dropbox. 
        dropboxr::upload_df_to_dropbox(lastfmUpload, "/R/lastfm/tracks.csv")
        
        return(lastfmUpload)
    } else {
        return(lastfmDownload)
    }
}
