## Want to try plotting ratings somehow

source('scripts/getPlex.R')
plex <- getPlex(T)

## Let's look at all the Pitchforks

# Pitchforks --------------------------------------------------------------


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


# Albums by Artist --------------------------------------------------------

## Compare albums by artist?
compArtist <- 'The Field'
plexArtist <- filter(plex, str_detect(artist, compArtist)) 
plexArtist %>% 
    group_by(album) %>% 
    arrange(album, discNum, trackNum) %>% 
    mutate(trackNum = row_number()) %>% 
    na.omit() %>% 
    ggplot(aes(x = trackNum, y = rating, group = album, color = album,
               text = str_c(track, ' (', rating/2, ')'))) +
    geom_point(alpha = 0.5) +
    geom_smooth(se = FALSE, span = 1)
plotly::ggplotly()


colors <- c('#FFFFFF', '#DDE2F9','#B5BEE7',
            '#8C99D6','#6475C4','#3B50B2')


plexArtist %>% 
    # group_by(album) %>% 
    # add_count() %>% 
    # filter(n > 1) %>% 
    # ungroup() %>% 
    # select(-n) %>% 
    mutate(rating = replace_na(rating, 0),
           rating = as.factor(rating/2),
           album = forcats::fct_rev(album)) %>% 
    plotly::plot_ly(y = ~album, x = ~trackNum, 
        type = 'scatter', mode = 'markers', 
        marker = list(size = 20, 
                      line = list(color = 'lightgrey', width = 1),
                      color = ~colors[as.numeric(as.character(rating))+1]),
        text = ~str_c(track, rating, sep = ": "), 
        hoverinfo = 'text') %>% 
    plotly::layout(showlegend = FALSE, 
           yaxis = list(title = NA), 
           xaxis = list(title = NA, zeroline = FALSE))


plexArtist %>% 
    group_by(album) %>% 
    mutate(y = n()) %>% 
    na.omit() %>% 
    mutate(x = n()) %>% 
    group_by(album, x, y) %>% summarise(avRat = mean(rating)/2) %>% 
    arrange(desc(avRat)) %>% 
    unite(x, y, col = 'n', sep = "/")

