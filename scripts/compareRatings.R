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
compArtist <- "The Strokes"
plexArtist <- filter(plex, str_detect(artist, compArtist)) %>% 
    filter(albumArtist != "Various Artists")
plexArtist %>% 
    group_by(album) %>% 
    arrange(album, discNum, trackNum) %>% 
    mutate(trackNum = row_number(),
           avgRat = round(mean(rating, na.rm = TRUE), 1)/2) %>% 
    na.omit() %>% 
    ggplot(aes(x = trackNum, y = rating/2, group = album, color = album,
               avg = avgRat,
               text = str_c(track, ' (', rating/2, ')'))) +
    geom_point(alpha = 0.5) +
    geom_smooth(se = FALSE, span = 1)
plotly::ggplotly(tooltip = 'avg')


colors <- c('#FFFFFF', '#DDE2F9','#B5BEE7',
            '#8C99D6','#6475C4','#3B50B2')


plexArtist %>% 
    # group_by(album) %>% 
    # add_count() %>% 
    # filter(n > 1) %>% 
    # ungroup() %>% 
    # select(-n) %>% 
    mutate(album = ifelse(discNum == 1, album, str_c(album, " (", discNum, ")"))) %>% 
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
    group_by(album, x, y) %>% summarise(avRat = mean(rating)/2, .groups = 'drop') %>% 
    arrange(avRat) %>% 
    mutate(album = str_c(album, "\n", "(", round(avRat, 1), ")")) %>% 
    mutate(album = forcats::fct_inorder(album)) %>% 
    mutate(compRat = (x/y)*avRat) %>% 
    select(-x, -y) %>% 
    gather(-album, key = series, value = Rating) %>% 
    ggplot(aes(x = album, y = Rating, group = series, fill = series)) +
    geom_col(position = 'identity', color = colors[6], size = 1) +
    scale_fill_manual(values = c(colors[1], colors[5])) + 
    coord_flip() +
    labs(x = NULL) +
    theme(legend.position = 'none')





# Compare completed albums ------------------------------------------------

albumList <- plex %>% 
    group_by(albumArtist, album) %>% 
    add_count(name = 'y') %>% 
    filter(!is.na(rating)) %>% 
    add_count(name = 'x') %>% 
    filter(x == y, y > 3) %>% 
    summarise(avRat = mean(rating)/2, .groups = 'drop') %>% 
    arrange(desc(avRat))
albumList %>%
    mutate(highlight = str_detect(albumArtist, compArtist)) %>% 
    unite(col = 'name', albumArtist:album, sep = " - ") %>% 
    mutate(name = forcats::fct_rev(forcats::fct_inorder(name))) %>% 
    ggplot(aes(y = avRat, x = name, fill = highlight)) + 
    scale_fill_discrete(compArtist) +
    geom_col() + 
    coord_flip()

albumList %>% 
    mutate(rank = row_number()) %>% 
    filter(str_detect(albumArtist, compArtist))