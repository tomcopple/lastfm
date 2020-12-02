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
compArtist <- 'Damien Jurado'
filter(plex, str_detect(artist, compArtist)) %>% 
    group_by(album) %>% 
    arrange(album, discNum, trackNum) %>% 
    mutate(trackNum = row_number()) %>% 
    na.omit() %>% 
    ggplot(aes(x = trackNum, y = rating, group = album, color = album,
               text = str_c(track, ' (', rating/2, ')'))) +
    geom_point(alpha = 0.5) +
    geom_smooth(se = FALSE, span = 1)
plotly::ggplotly()

filter(plex, str_detect(artist, compArtist)) %>% 
    group_by(album) %>% 
    mutate(y = n()) %>% 
    na.omit() %>% 
    mutate(x = n()) %>% 
    group_by(album, x, y) %>% summarise(avRat = mean(rating)/2) %>% 
    arrange(desc(avRat)) %>% 
    unite(x, y, col = 'n', sep = "/")

plex %>% na.omit() %>% count(artist, sort = T)
