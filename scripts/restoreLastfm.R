# Backup script if you need to restore all scrobbles for some reason

library(tidyverse);library(lubridate);library(jsonlite);library(httr2);library(here)

# Get credentials from 
user <- Sys.getenv('LASTFM_USER')
api <- Sys.getenv('LASTFM_APIKEY')

# Set lastfm base url
baseurl <- paste0(
    "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=", user,
    "&api_key=", api, "&format=json&limit=200&page="
)


## Find out how many responses in total
res1 <- httr2::request(glue::glue("http://ws.audioscrobbler.com/2.0/?method=user.getinfo&user={user}&api_key={api}&format=json"))
resp1 <- httr2::req_perform(res1)

howMany <- resp1 %>% 
    httr2::resp_body_json() %>% 
    pluck('user', 'playcount') %>% 
    as.numeric()

# Initiate data frame
restore <- data.frame(    )

# So how many pages to get? Get 200 per page, so 
# Get 200 per page and need ~120,000 responses, so ~600 pages in total?
# Find the highest page?
pages <- ceiling(howMany/200)
i <- 1
keepgoing = TRUE

## Can also tell function to stop once it hits a certain date
stopDate <- lubridate::ymd("2006-01-01")


## NB If it crashes, check i then start again including that number

while (keepgoing) {
    url <- str_c(baseurl, i)
    
    print (glue::glue("This may take a while, don't panic... [{i}/{pages}]"))
    
    ## send httr GET request and format response
    res1 <- httr2::request(url) %>% 
        req_retry(max_tries = 3, backoff = ~30)
    resp1 <- httr2::req_perform(res1) 
    
    tracksRaw <- resp1 %>% 
        httr2::resp_body_string() %>% 
        jsonlite::fromJSON(simplifyDataFrame = T, flatten = T) %>% 
        pluck('recenttracks', 'track')
    
    tracksDF <- tracksRaw %>% 
        select(track = name, artist = `artist.#text`, album = `album.#text`,
               dateRaw = `date.#text`) %>% 
        mutate(date = parse_date_time(dateRaw, "d b Y, H:M"), .keep = 'unused') %>% 
        filter(!is.na(date)) %>% 
        as_tibble()
    
    restore <- bind_rows(restore, tracksDF) %>% 
        arrange(desc(date))
    
    i <- i + 1
    
    if( i > pages) {
        keepgoing <- FALSE
    }
    
    if(min(restore$date) <= stopDate) {
        keepgoing <- FALSE
    }
    
}

## Remove duplicates (i.e. same track in same minute)
restoreFinal <- as_tibble(restore) %>% 
    filter(date >= stopDate) %>% 
    distinct()

## Import current lastfm data
source('scripts/getLastfm.R')
lastfm <- getLastfm(F)

## And replace with new data
newLastfm <- filter(lastfm, date < min(restoreFinal$date)) %>% 
    bind_rows(restoreFinal) %>%
    arrange(date) %>% 
    distinct()

## Save and upload a backup copy and the master copy
write.csv(newLastfm, file = here::here('tempData', str_c(today(), '-restoreLastfm.csv')), 
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(newLastfm, file = here::here('tempData', 'tracks.csv'), 
          row.names = FALSE, fileEncoding = "UTF-8")

## Upload to Dropbox ---- 
dropboxClient <- oauth_client(
    id = Sys.getenv('DROPBOX_KEY'),
    secret = Sys.getenv('DROPBOX_SECRET'),
    token_url = "https://api.dropboxapi.com/oauth2/token",
    name = 'Rstudio_TC'
)
dropboxToken <- readRDS(here::here('dropbox.RDS'))

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
