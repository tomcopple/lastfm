# Backup script if you need to restore all scrobbles for some reason

library(tidyverse);library(lubridate);library(httr);library(jsonlite)
library(rdrop2);library(here)

# Get credentials from 
user <- Sys.getenv('LASTFM_USER')
api <- Sys.getenv('LASTFM_APIKEY')

# Set lastfm base url
baseurl <- paste0(
    "http://ws.audioscrobbler.com/2.0/?method=user.getrecenttracks&user=", user,
    "&api_key=", api, "&format=json&limit=200&page="
)


## Find out how many responses in total
res1 <- httr::GET(glue::glue("http://ws.audioscrobbler.com/2.0/?method=user.getinfo&user={user}&api_key={api}&format=json"))
httr::stop_for_status(res1)
howMany <- res1 %>% httr::content() %>% 
    pluck('user') %>% 
    pluck('playcount') %>% 
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
    res1 <- httr::GET(url)
    httr::stop_for_status(res1)
    res2 <- httr::content(res1, as = 'text')    
    res3 <- fromJSON(res2, simplifyDataFrame = T, flatten = T) %>% 
        pluck('recenttracks') %>% 
        pluck('track')
    res4 <- res3 %>% 
        select(track = name, artist = `artist.#text`, album = `album.#text`,
               dateRaw = `date.#text`) %>% 
        mutate(date = parse_date_time(dateRaw, "d b Y, H:M"), .keep = 'unused') %>% 
        filter(!is.na(date)) %>% 
        as_tibble()
    
    restore <- bind_rows(restore, res4) %>% 
        arrange(desc(date))
    
    i <- i + 1
    
    if( i > pages) {
        keepgoing <- FALSE
    }
    
    if(min(restore$date) <= stopDate) {
        keepgoing <- FALSE
    }
    
}

restoreFinal <- as_tibble(restore) %>% 
    filter(date >= stopDate)

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
write.csv(newLastfm, file = 'tracks.csv', 
          row.names = FALSE, fileEncoding = "UTF-8")


rdrop2::drop_upload(file = here::here('tempData', str_c(today(), '-restoreLastfm.csv')),
                    path = 'R/lastfm')

rdrop2::drop_upload(file = 'tracks.csv',
                    path = 'R/lastfm')
