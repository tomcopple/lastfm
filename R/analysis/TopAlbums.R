## Top albums to re-listen to

library(tidyverse);library(httr);library(lubridate);library(rdrop2)

source(here::here("R", "bootstrap.R"))
source_project("R", "lib", "getPlexRatings.R")
source_project("R", "lib", "getLastfm.R")
source_project("R", "lib", "getSlugs.R")

tracks <- getLastfm(T)
plex <- getPlexRatings(T, F, F)

## What are the best albums? Top avg ratings, most 4/5* songs? Use a mix
topAlbums <- 

topAlbums <- albumRatings %>% 
    filter(x == y) %>% 
    arrange(desc(avRat)) %>% 
    top_n(n = 25, wt = avRat) %>% 
    select(albumArtist, album, avRat, y)  %>% 
    bind_rows(plex %>% 
                  group_by(albumArtist, album) %>% 
                  add_count(name = 'y') %>% 
                  filter(!is.na(rating)) %>% 
                  mutate(avRat = mean(rating)/2) %>% 
                  filter(rating > 6) %>% 
                  count(albumArtist, album, avRat, y, sort = T) %>% 
                  filter(str_detect(album, 'Pitchfork', negate = T),
                         str_detect(album, 'Motown', negate = T),
                         str_detect(album, 'Singles Collection', T)) %>% 
                  ungroup() %>% 
                  top_n(n = 25, wt = n) %>% 
                  select(albumArtist, album, avRat, y)) %>% 
    distinct(albumArtist, album, avRat, y) %>% 
    rename(artist = albumArtist) %>% 
    arrange(desc(avRat)) %>% 
    mutate(joinArtist = getSlugs(artist),
           joinAlbum  = getSlugs(album))

topAlbums

lastPlayed <- tracks %>% 
    mutate(artist = ifelse(album == 'Rushmore', 'Various Artists', artist)) %>% 
    mutate(joinArtist = getSlugs(artist),
           joinAlbum = getSlugs(album)) %>% 
    filter(joinArtist %in% topAlbums$joinArtist, 
           joinAlbum %in% topAlbums$joinAlbum) %>% 
    mutate(date = as_date(date)) %>% 
    group_by(artist, album, date, joinArtist, joinAlbum) %>% 
    count() %>% 
    left_join(topAlbums %>% select(-artist, -album), by = c('joinArtist', 'joinAlbum')) %>% 
    mutate(n2 = ifelse(y < 10, y/2, 5)) %>% 
    filter(n >= n2) %>% 
    select(-n2) %>% 
    group_by(artist, album) %>% 
    filter(date == max(date)) %>%
    select(-joinArtist, -joinAlbum) %>% 
    arrange(date)

## Missing some
# anti_join(topAlbums, lastPlayed, by = c('joinArtist', 'joinAlbum'))

lastPlayed
tail(lastPlayed)

