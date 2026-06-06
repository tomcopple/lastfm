## Query Plex API?

getPlexRatings <- function(refresh = FALSE, printTree = FALSE) {
    
    suppressMessages({
            library(tidyverse);library(httr);library(lubridate);library(treemapify);library(dropboxr)
    })
    
    source(here::here("R", "bootstrap.R"))
    source_project("R", "lib", "getPlex.R")
    
    ## Music is section 5, need to search '10' to get all tracks
    if(refresh) {
        
        print("Downloading plex data")
        plex <- getPlex(refresh = TRUE)
        
        .GlobalEnv$plex <- plex
        
        if(printTree) { 
            
            print("Producing treemap")
            treemapData <- plex %>% 
                select(track, album, rating) %>% 
                mutate(rating = replace_na(rating, 0),
                       album = replace_na(album, "Unknown"),
                       track = replace_na(track, "Unknown"),
                       rating = floor(rating / 2),
                       n = 1) %>% 
                mutate(rating = forcats::as_factor(rating))  
            
            treemap <- ggplot(treemapData, aes(area = n, fill = rating, subgroup = album)) +
                geom_treemap() +
                geom_treemap_subgroup_border(size = 1) +
                scale_fill_brewer(palette = 'Blues')

            progress <- plex %>% 
                summarise(n1 = n(), n2 = sum(!is.na(rating))) %>% 
                transmute(perc = n2 / n1, tot = 1 - perc) %>% 
                tidyr::pivot_longer(cols = everything(), names_to = "key", values_to = "value") %>% 
                mutate(
                    key = factor(key, levels = c("tot", "perc")),
                    label = ifelse(key == "perc", str_c(round(100 * value, 0), "%"), "")
                ) %>% 
                ggplot(aes(x = 1, y = value, fill = key, alpha = key)) + 
                geom_col(position = 'fill', color = "#7AADD2") + 
                coord_flip() + 
                scale_fill_manual(values = c("tot" = "#CADAED", "perc" = "#7AADD2")) + 
                scale_alpha_manual(values = c("tot" = 0.5, "perc" = 1)) + 
                geom_label(aes(label = label, y = 0.9), size = 6, fill = 'white', label.size = 0, fontface = 2) + 
                theme_void() + 
                theme(
                    legend.position = 'none',
                    plot.background = element_rect(fill = 'white', color = 'white'),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    panel.border = element_blank(),
                    plot.margin = margin(10, 8, 2, 8)
                )
            
            treeFilename <- str_glue("{format(now(), '%Y-%m-%d-%H-%M-%S')}-treemap.png")
            
            print("Uploading treemap to Dropbox")
            
            dropboxr::dropbox_auth()
            temp_file <- tempfile(fileext = ".png")
            png(filename = temp_file, width = 10.5, height = 9.14, units = "in", res = 300)
            grid::grid.newpage()
            grid::pushViewport(grid::viewport(
                layout = grid::grid.layout(nrow = 2, ncol = 1, heights = grid::unit(c(0.08, 0.92), "npc"))
            ))
            print(progress, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
            print(treemap, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
            grid::popViewport()
            dev.off()
            
            dropboxr::upload_dropbox_file(temp_file, str_glue("/R/lastfm/{treeFilename}"), mode= "overwrite")
            unlink(temp_file)
        }
    } else {
        plex <- getPlex(F)
    }
    
    ratings <- plex %>% 
        group_by(albumArtist, album) %>% 
        add_count(name = 'y') %>% 
        filter(y > 3, !is.na(rating)) %>% 
        add_count(name = 'x') %>% 
        filter(x > 2) %>% 
        group_by(albumArtist, album, x, y) %>% 
        summarise(avRat = mean(rating)/2, .groups = 'drop') %>% 
        unite(col = 'n', sep = '/', remove = FALSE, c(x, y)) %>% 
        arrange(desc(avRat))
        
        print(ratings)
        .GlobalEnv$albumRatings <- ratings

    return(plex)
}

plex <- getPlexRatings(T, T)

str_c(100*round(1-(sum(is.na(plex$rating))/nrow(plex)),2), "% complete")

albumRatings %>%
    filter(x == y)

plex %>% 
    filter(str_detect(album, 'Complete Motown')) %>% 
    mutate(title = str_sub(album, start = -4)) %>% 
    arrange(title) %>% 
    mutate(title = forcats::fct_inorder(title)) %>% 
    ggplot(aes(x = trackNum,y = rating,color = title, group = title,text = track)) + 
    geom_point(alpha = 0.6) + 
    stat_smooth(se = FALSE)

plotly::ggplotly()

plex %>% 
    filter(str_detect(album,'Complete Motown')) %>% 
    filter(!is.na(rating)) %>% 
    group_by(album) %>% 
    summarise(avrat = mean(rating)) %>% 
    mutate(album = str_sub(album,-4)) %>% 
    arrange(album) %>% 
    ggplot(aes(x = avrat,y = album)) +geom_col(fill = 'lightblue')

