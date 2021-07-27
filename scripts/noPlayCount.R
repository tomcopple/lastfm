## Check for plex tracks with no play count

library(tidyverse);library(httr);library(lubridate);library(rdrop2)

source('scripts/getPlex.R')
source('scripts/getLastfm.R')

plex <- getPlex(T) 
lastfm <- getLastfm()

p1 <- plex %>% mutate_if(is.character, str_to_lower) %>% 
    mutate(artist = ifelse(is.na(artist), albumArtist, artist))
l1 <- lastfm %>% mutate_if(is.character, str_to_lower)


p1 %>% 
    left_join(l1 %>% 
                  count(artist, track)) %>% 
    filter(is.na(n)) %>%
    select(-n) %>% 
    count(artist, sort = T)
    
noPlays <- p1 %>% 
    left_join(lastfm %>% 
                  count(artist, track)) %>% 
    filter(is.na(n)) %>%
    select(-n)

## Also check for most played tracks not in Plex
l1 %>% count(track, artist, sort = T) %>% 
    anti_join(p1,by = c('artist', 'track'))

plex %>% filter(str_detect(track, 'Antichrist'))


# Missing playcounts ------------------------------------------------------


p1 %>% 
    filter(str_detect(album, "^pitchfork")) %>% 
    left_join(l1 %>% count(artist, track)) %>% 
    filter(is.na(n)) %>% 
    arrange(artist) %>% 
    select(artist, track, album, rating)
