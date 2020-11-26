## Want to try plotting ratings somehow

source('scripts/getPlex.R')
plex <- getPlex(T)

## Let's look at all the Pitchforks

pf <- filter(plex, str_detect(album, "Pitchfork.*Tracks"))

pf %>% 
    mutate(year = str_sub(album, -4)) %>% 
    mutate(rating = rating/2) %>% 
    mutate(text = str_c(artist, " - ", track,  " (", year, ")", "<br>", "Rating: ", rating)) %>% 
    ggplot(aes(x = trackNum, y = rating, group = year, color = year, text = text)) + 
    geom_point(alpha = 0.5) + 
    geom_smooth(se = FALSE, span = 1)

plotly::ggplotly(tooltip = c('text'))

pf %>% group_by(album) %>% summarise(avRat = mean(rating, na.rm = T)) %>% 
    arrange(desc(avRat))

pf %>% 
    group_by(album) %>% 
    mutate(nMax = n()) %>% 
    mutate(rated = ifelse(is.na(rating), FALSE, TRUE)) %>% 
    mutate(nRat = sum(rated)) %>% 
    distinct(album, nRat, nMax)

pf %>% filter(!is.na(rating)) %>% count(album,sort = T)

## Compare albums by artist?

filter(plex, str_detect(artist, "Tom Waits")) %>% 
    group_by(album) %>% 
    arrange(album, discNum, trackNum) %>% 
    mutate(trackNum = row_number()) %>% 
    na.omit() %>% 
    ggplot(aes(x = trackNum, y = rating, group = album, color = album)) +
    geom_point(alpha = 0.5) +
    geom_smooth(se = FALSE, span = 1)
plotly::ggplotly()

filter(plex, str_detect(artist, "Tom Waits")) %>% 
    group_by(album) %>% 
    arrange(album, discNum, trackNum) %>% 
    mutate(trackNum = row_number()) %>% 
    group_by(album) %>% 
    mutate(x = ifelse(is.na(rating), 0, 1)) %>% 
    mutate(x = sum(x), y = n()) %>% 
    na.omit() %>% 
    group_by(album, x, y) %>% summarise(avRat = mean(rating)) %>% 
    arrange(desc(avRat))

plex %>% na.omit() %>% count(artist, sort = T)
