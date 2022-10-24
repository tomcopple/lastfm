## New albums 2022

library(tidyverse);library(lubridate)
source('scripts/getLastfm.R')

tracks <- getLastfm(T)

played22 <- tracks %>% filter(year(date) == 2022) %>%
    na.omit() %>% 
    unite(artist, album, col = 'album', sep = " - ") %>% 
    count(album) %>% 
    filter(n >= 10) %>% 
    arrange(desc(n))

playedBefore <- tracks %>% 
    filter(year(date) < 2021) %>% 
    unite(artist, album, col = 'album', sep = " - ") %>% 
    count(album) %>% 
    filter(n >= 5) %>%
    arrange(desc(n))

anti_join(played22, playedBefore, by = 'album') %>% 
    filter(str_detect(album, 'Motown Singles', negate = T),
           str_detect(album, 'Classical Lullabies', negate = T),
           str_detect(album, 'Highway Strip', negate = T),
           str_detect(album, 'Superworm', negate = T)) %>% 
    arrange(n)
