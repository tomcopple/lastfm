## Try to get tracks from 2020 playlist

library(tidyverse);library(httr);library(lubridate);library(treemapify)

token <- '2CdDkLKF5xY27xxuxHB5'

slug <- "http://192.168.1.99:32400/playlists/108695/items"

## Download playlist
play20raw <- content(httr::GET(slug, add_headers("X-Plex-Token" = token)), type = 'text') %>% 
    jsonlite::fromJSON() %>% 
    magrittr::extract2('MediaContainer') %>% 
    magrittr::extract2('Metadata')
play20 <- play20raw %>% select(
    artist    = grandparentTitle, 
    album       = parentTitle, 
    track       = title, 
    rating      = userRating,
    key         = ratingKey,
    artistKey   = grandparentRatingKey,
    albumKey    = parentRatingKey,
    trackNum       = index,
    discNum        = parentIndex
) %>% 
    as_tibble()

play20 %>% 
    add_count(artist, album) %>% 
    na.omit() %>% 
    group_by(artist, album, n) %>%  
    summarise(avRat = mean(rating)/2, 
              n1 = n()) %>% 
    arrange(desc(avRat)) %>% 
    unite(n1, n, col = 'n', sep = "/")
    
play20 %>% filter(rating > 7)

colPal <- c('#FFFFFF', colorRampPalette(c("#C1D1F4", "#003EBB"))(4))

play20 %>% 
    select(album, rating) %>% 
    mutate(rating = replace_na(rating, 0),
           rating = round(rating / 2) * 2,
           n = 1) %>% 
    mutate(rating = forcats::as_factor(rating)) %>% 
    ggplot(aes(area = n, fill = rating, subgroup = album)) +
    geom_treemap() +
    geom_treemap_subgroup_border(size = 1) +
    scale_fill_manual(values = colPal)

play20 %>% 
    mutate(rating = replace_na(rating, 0)) %>% 
    mutate(rating = as.factor(rating/2)) %>% 
    mutate(title = str_c(artist, album, sep = ' - '),
           title = forcats::fct_rev(title)) %>% 
    ggplot(aes(x = trackNum, y = title, 
               fill = rating, group = rating)) +
    geom_point(shape = 21, size = 5) +
    scale_fill_manual("", values = colPal) +
    xlab(NULL) + ylab(NULL)


play20 %>% 
    mutate(rating0 = replace_na(rating, 0)/2, 
           rating = rating/2) %>% 
    mutate(text = str_c(artist, " - ", track, "<br>", "Rating: ", rating0)) %>% 
    ggplot(aes(x = trackNum, y = rating0, group = album, color = album, text = text)) + 
    geom_point(alpha = 0.5) + 
    geom_smooth(se = FALSE, span = 1, aes(y = rating))
plotly::ggplotly(tooltip = 'text')
