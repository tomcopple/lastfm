## Import lastfm csv from https://benjaminbenben.com/lastfm-to-csv/
## Always get errors, so maybe download a few times, then filter for
## unique values

## Check against old data, shouldn't be less, but depends. 

## Files are saved as tomcopple, tomcopple2, tomcopple3 in tempData

## Looks like there are some duplicates on Lastfm, remove from file?

library(tidyverse);library(lubridate)

## Import old data for comparison
source('scripts/getLastfm.R')
lastOld <- getLastfm(F)

## Import new files
lastNew1 <- read_csv('tempData/tomcopple.csv', col_names = FALSE)
lastNew2 <- read_csv('tempData/tomcopple2.csv', col_names = FALSE) 
lastNew3 <- read_csv('tempData/tomcopple3.csv', col_names = FALSE)

lastNew <- bind_rows(lastNew1, lastNew2, lastNew3) %>% 
    select(track = X3, artist = X1, album = X2, date = X4) %>% 
    mutate(date = lubridate::dmy_hm(date)) %>% 
    distinct(track, artist, album, date)

## NB Duplicates here
lastOld %>% group_by(track, artist, album, date) %>% 
    filter(n() > 1)


full_join(
    lastNew %>% count(date(date), name = 'new'),
    lastOld %>% count(date(date), name = 'old')
) %>% 
    mutate(check = new != old) %>% 
    filter(check) %>% 
    mutate(diff = old - new) %>% 
    arrange(desc(abs(diff)))


## If it doesn't look too bad, reupload back to dropbox
write.csv(lastNew, file = "tempData/tracks.csv", row.names = FALSE, fileEncoding = "UTF-8")
rdrop2::drop_upload(file = "tempData/tracks.csv", path = "R/lastfm")
