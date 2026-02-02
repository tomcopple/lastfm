## Query Plex API?

getPlexRatings <- function(refresh = FALSE, printTree = FALSE) {
    
    library(tidyverse);library(httr);library(lubridate);library(treemapify);library(dropboxr)
    
    source('scripts/getPlex.R')
    
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
                       rating = round(rating / 2),
                       n = 1) %>% 
                mutate(rating = forcats::as_factor(rating))  
            
            treemap <- ggplot(treemapData, aes(area = n, fill = rating, subgroup = album)) +
                geom_treemap() +
                geom_treemap_subgroup_border(size = 1) +
                scale_fill_brewer(palette = 'Blues')
            
            treeFilename <- str_glue("{format(now(), '%Y-%m-%d-%H-%M-%S')}-treemap.png")
            
            print("Uploading treemap to Dropbox")
            
            dropboxr::dropbox_auth()
            temp_file <- tempfile(fileext = ".png")
            ggsave(plot = treemap,
                   filename = temp_file,
                   width = 10.5, height = 9.14, units = "in")
            
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
