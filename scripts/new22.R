## New albums 2022

library(tidyverse);library(lubridate)
source('scripts/getLastfm.R')

tracks <- getLastfm(T)

played22 <- tracks %>% filter(year(date) == 2022) %>%
    na.omit() %>% 
    unite(artist, album, col = 'album', sep = " - ") %>% 
    count(album) %>% 
    filter(n >= 9) %>% 
    arrange(desc(n))

playedBefore <- tracks %>% 
    filter(year(date) < 2022) %>% 
    unite(artist, album, col = 'album', sep = " - ") %>% 
    count(album) %>% 
    filter(n >= 5) %>%
    arrange(desc(n))

anti_join(played22, playedBefore, by = 'album') %>% 
    filter(str_detect(album, 'Motown Singles', negate = T),
           str_detect(album, 'Classical Lullabies', negate = T),
           str_detect(album, 'Highway Strip', negate = T),
           str_detect(album, 'Superworm', negate = T),
           ## Dargz is all singles
           str_detect(album, 'Dargz', negate = T),
           ## Kaytranada is a 2021 EP
           str_detect(album, 'KAYTRANADA', negate = T),
           str_detect(album, 'Arlo Parks', negate = T),
           str_detect(album, 'Please Do Not Lean', negate = T),
           str_detect(album, 'Stereolab', negate = T),
           str_detect(album, 'Drive My Car', negate = T),
           str_detect(album,  'Surface', negate = T)) %>% 
    arrange(n)

    