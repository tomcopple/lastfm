## Query Plex API?

getPlexRatings <- function(refresh = FALSE, write = FALSE, printTree = FALSE) {
    library(tidyverse);library(httr);library(lubridate);library(rdrop2);library(treemapify)
    source('scripts/getPlex.R')
    token <- 'ABhPTJsJFC1CsCPKzzhb'
    
    ## Music is section 5, need to search '10' to get all tracks
    if(refresh) {
        
        print("Downloading plex data")
        source('scripts/getPlex.R')
        plex <- getPlex(refresh = TRUE)
        
        .GlobalEnv$plex <- plex
        
        print("Producing treemap")
        treemapData <- plex %>% 
            select(album, rating) %>% 
            mutate(rating = replace_na(rating, 0),
                   rating = round(rating / 2) * 2,
                   n = 1) %>% 
            mutate(rating = forcats::as_factor(rating))  
        treemap <- ggplot(treemapData, aes(area = n, fill = rating, subgroup = album)) +
                geom_treemap() +
                geom_treemap_subgroup_border(size = 1) +
                scale_fill_brewer()
        if(printTree) { print(treemap) }
        treeFilename <- here::here('tempData', str_c(
            str_replace_all(lubridate::now(), pattern = "[:\\s]", "-"), '-treemap.png'))
        ggsave(plot = treemap,
               filename = treeFilename,
               width = 10.5, height = 9.14, units = "in")
        rdrop2::drop_upload(file = treeFilename,
                            path = 'R/lastfm/')
        
        
        
        
    } else {
        plex <- getPlex(F)
    }
    
    ## And save ratings
    ratedDF <- plex %>% 
        filter(!is.na(rating))
    
    ## Save master copy of ratings
    if(write) {
        print("Saving backup copy of data")
        write_csv(ratedDF, here::here('tempData', 'plexMasterRatings.csv'))
        rdrop2::drop_upload(file = here::here('tempData', 'plexMasterRatings.csv'), 
                            path = 'R/lastfm/')
        
    }
        
    ratings <- plex %>% 
        group_by(albumArtist, album) %>% 
        add_count(name = 'y') %>% 
        filter(y > 2, !is.na(rating)) %>% 
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
plex <- getPlexRatings(T, T, T)

albumRatings %>% 
    filter(x == y)
