## Check for plex tracks with no play count

library(tidyverse);library(httr);library(lubridate);library(rdrop2)

source('scripts/getPlex.R')
source('scripts/getLastfm.R')

plex <- getPlex(T) 
lastfm <- getLastfm()

plex %>% 
    mutate(artist = ifelse(is.na(artist), albumArtist, artist)) %>% 
    left_join(lastfm %>% 
                  count(artist, track)) %>% 
    filter(is.na(n)) %>%
    select(-n) %>% 
    count(artist, sort = T)
    
noPlays <- plex %>% 
    mutate(artist = ifelse(is.na(artist), albumArtist, artist)) %>% 
    mutate(artist = str_to_lower(artist), track = str_to_lower(track)) %>% 
    left_join(lastfm %>% 
                  count(artist, track) %>% 
                  mutate(artist = str_to_lower(artist), track = str_to_lower(track))) %>% 
    filter(is.na(n)) %>%
    select(-n)

## Also check for most played tracks not in Plex
lastfm %>% count(track, artist, sort = T) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    anti_join(plex %>% 
                  mutate_if(is.character, str_to_lower),
              by = c('artist', 'track'))

plex %>% filter(str_detect(track, 'Antichrist'))


# Missing playcounts ------------------------------------------------------


plex %>% 
    filter(str_detect(album, "^Pitchfork")) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    left_join(lastfm %>% count(artist, track) %>% 
                  mutate_if(is.character, str_to_lower)) %>% 
    filter(is.na(n)) %>% 
    arrange(artist) %>% 
    select(artist, track, album, rating)
