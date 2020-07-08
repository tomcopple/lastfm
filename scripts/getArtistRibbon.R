## Stacked ribbon chart of artist plays by album?
## Possibly to include in shiny app

## Download lastfm first
library(tidyverse);library(httr);library(purrr);library(stringr);library(spotifyr)
library(here);library(lubridate)
source(here::here('scripts', 'getLastfm.R'))

tracks <- getLastfm(T)

getArtist <- 'Eels'

artistTracks <- filter(tracks, artist == getArtist) %>% 
    ## track title, to get rid of (2019 remaster), (demo), (alternate) etc
    mutate(album = str_remove_all(album, "\\s[\\(\\-\\[].*")) %>% 
    mutate(album = stringr::str_to_title(album)) %>% 
    mutate(date = as_date(date))
    

## Consolidate albums as per app
albumList <- artistTracks %>% 
    distinct(track, artist, album) %>% 
    na.omit() %>% 
    group_by(album) %>% 
    mutate(n = n()) %>% 
    group_by(track) %>% 
    arrange(desc(n)) %>% 
    slice(1) %>% 
    distinct(track,album) %>% ungroup() %>% 
    mutate(album = forcats::fct_lump_n(album, n = 9, ties.method = 'last'))

albumClean <- full_join(artistTracks %>% select(-album),
                          albumList, by = 'track') %>% 
    mutate(album = forcats::fct_other(album, drop = ''),
           album = forcats::fct_explicit_na(album, na_level = 'Others'))
    
## Separate into columns? How does this work?
albumPlays <- albumClean %>%
    count(album, date) %>% 
    spread(key = album, value = n) %>% 
    full_join(., tibble(date = seq.Date(from = min(albumClean$date),
                                        to = max(albumClean$date) + 90,
                                        by = 1)), by = 'date') %>% 
    arrange(date) %>% 
    gather(-date, key = album, value = n) %>% 
    mutate(n = replace_na(n, 0)) %>% 
    arrange(album, date) %>% 
    group_by(album) %>% 
    mutate(plays = c(cumsum(n[1:89]),
                     zoo::rollsum(n, k = 90, align = 'right')))
    

plot_ly(albumPlays, x = ~date, y = ~plays, color = ~album, type = 'bar',
        colors = 'Spectral') %>% 
    plotly::layout(xaxis = list(title = ''),
                   yaxis = list(title = 'Plays'),
                   legend = list(orientation = 'h'),
                   barmode = 'stack')



top10plays %>% 
    group_by(newCol) %>% 
    plot_ly(x = ~date, y = ~plays, color = ~newCol, type = "bar", 
            # mode ="lines", fill = "tozeroy", 
            colors = "Spectral", 
            text = ~str_c(album, "<br>Plays: ", plays), hoverinfo = "text") %>% 
    plotly::layout(xaxis = list(title = ""),
                   yaxis = list(title = "Monthly plays"),
                   title = str_c("Top ten ", tolower(input$chooseType), " in ", input$chooseYear),
                   legend = list(orientation = "h", xanchor = "center", x = 0.5,
                                 yanchor = "top", y = 0))