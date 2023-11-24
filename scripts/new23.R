## New albums for 2023

## New albums 2022

library(tidyverse);library(lubridate)
source('scripts/getLastfm.R')

tracks <- getLastfm(T)

new23 <- tracks %>% 
    mutate(year = year(date)) %>%
    mutate(across(where(is.character), str_to_title)) %>% 
    group_by(artist, album) %>%
    count(track, year, name = 'trackPlays') %>%
    group_by(track) %>%
    top_n(n = -1, wt = year) %>%
    group_by(artist, album) %>% add_count(year, name = 'newTracks') %>%
    filter(year == 2023, newTracks > 5) %>% 
    group_by(artist, album, newTracks) %>% 
    tally(trackPlays, name = 'albumPlays') %>% 
    ungroup() %>% 
    arrange(desc(albumPlays)) %>% na.omit()

new23 %>% 
    filter(str_detect(album, 'Everything Everywhere All', negate = T),
           album != 'The Low End Theory',
           str_detect(album, 'Ethiopiques', negate = T),
           album != 'Heavy Elevator',
           str_detect(album, 'The Beginning Stages', negate = T),
           album != 'Uprooted',
           album != 'One Year',
           album != 'Sleep',
           album != 'Mia Gargaret',
           str_detect(album, 'Bait Ones', negate = T),
           str_detect(album, 'Sophtware', negate = T)) %>% 
    View()
View(new23)
