## animated bar chart race
## Top 10 albums of 2023?

library(tidyverse);library(here);library(gganimate);require(gifski)

source(here::here("R", "bootstrap.R"))
source_project("R", "lib", "getLastfm.R")
lastfm <- getLastfm()

getYear <- filter(lastfm, year(date) == 2023)
getYear <- lastfm

## Think need to do it in two stages, get list of albums to include first
## Then create a running daily count

getAlbums <- getYear %>% 
    na.omit() %>% 
    mutate(date = floor_date(date, unit = 'days')) %>% 
    unite(artist, album, col = 'name', sep = " \n ") %>% 
    count(name, date) %>% 
    arrange(date) %>% 
    group_by(name) %>% 
    mutate(cumsum = cumsum(n)) %>% 
    group_by(date) %>% 
    slice_max(order_by = cumsum, n = 15, with_ties = FALSE) %>% 
    ungroup() %>% 
    distinct(name) %>% pull(name)

raceData <- getYear %>% 
    na.omit() %>% 
    select(-track) %>% 
    unite(artist, album, col = 'name', sep = " \n ") %>% 
    filter(name %in% getAlbums) %>% 
    mutate(date = floor_date(date, unit = 'days')) %>% 
    count(date, name) %>% 
    spread(key = name, value = n) %>% 
    gather(-date, key = name, value = n) %>% 
    mutate(n = replace_na(n, 0)) %>% 
    group_by(name) %>% 
    arrange(date) %>% 
    mutate(cumsum = cumsum(n)) %>% 
    group_by(date) %>% 
    slice_max(order_by = cumsum, n = 15, with_ties = FALSE) %>% 
    mutate(rank = rank(cumsum, ties.method = 'last')) %>% 
    ungroup() 

p <- raceData %>% 
    # mutate(name = stringr::str_sub(name, start = 1, end = 60)) %>% 
    # filter(date == min(date)) %>% 
    ggplot(aes(x = rank, y = cumsum, fill = name)) +
    geom_tile(aes(y = cumsum/2, height = cumsum), width = 0.9) +
    coord_flip(clip = 'off', expand = FALSE) +
    geom_text(aes(label = name), col = 'white', hjust = 'right', nudge_y = -1,
              size = 4) +
    geom_text(aes(label = cumsum), hjust = 'left', nudge_y = 1) +
    ylab('Total plays') + xlab(NULL) +
    ggtitle('Top album plays')+
    # scale_y_continuous(limits = c(-10, 200)) +
    scale_x_discrete("") +
    theme_minimal() +
    theme(legend.position = 'none') +
    labs(subtitle = "{format.Date(closest_state, '%b')}") +
    gganimate::transition_states(date, wrap = FALSE, state_length = 3)

gganimate::animate(p, end_pause = 60, width = 800, height = 550, 
                   duration = 60, fps = 10)
