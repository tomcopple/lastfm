## New albums for 2023

library(tidyverse);library(lubridate)
source('scripts/getLastfm.R')

tracks <- getLastfm(T)

<<<<<<< HEAD
getNew <- function(enterYear = year(today())) {
    
    newTracks <- tracks %>% 
        mutate(year = year(date)) %>% 
        mutate(across(where(is.character), str_to_title)) %>% 
        group_by(artist, album) %>%
        count(track, year, name = 'trackPlays') %>%
        group_by(track) %>%
        top_n(n = -1, wt = year) %>%
        group_by(artist, album) %>% add_count(year, name = 'newTracks') %>%
        filter(year == enterYear, newTracks > 5) %>% 
        group_by(artist, album, newTracks) %>% 
        tally(trackPlays, name = 'albumPlays') %>% 
        ungroup() %>% 
        arrange(desc(albumPlays)) %>% na.omit()
    
    return(newTracks)
}

>>>>>>> c729362c645f0bf71b22289b6a5b6bb896f29a37

getNew() %>% View()
