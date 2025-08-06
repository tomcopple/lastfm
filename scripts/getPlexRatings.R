## Query Plex API?

getPlexRatings <- function(refresh = FALSE, write = FALSE, printTree = FALSE) {
    library(tidyverse);library(httr);library(lubridate);library(treemapify)
    source('scripts/getPlex.R')
    token <- "o8xPSiPMQtky4cxEtdKW"
    
    
    
    ## Music is section 5, need to search '10' to get all tracks
    if(refresh) {
        
        print("Downloading plex data")
        source('scripts/getPlex.R')
        plex <- getPlex(refresh = TRUE)
        
        .GlobalEnv$plex <- plex
        
        
        if(printTree) { 
            
            dropboxClient <- oauth_client(
                id = Sys.getenv('DROPBOX_KEY'),
                secret = Sys.getenv('DROPBOX_SECRET'),
                token_url = "https://api.dropboxapi.com/oauth2/token",
                name = 'Rstudio_TC'
            )
            dropboxToken <- readRDS(here::here('dropbox.RDS'))
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
                
            ggsave(plot = treemap,
                   filename = here::here('tempData', treeFilename),
                   width = 10.5, height = 9.14, units = "in")
            
            reqUpload <- request('https://content.dropboxapi.com/2/files/upload/') %>% 
                req_oauth_refresh(client = dropboxClient, 
                                  refresh_token = dropboxToken$refresh_token) %>% 
                req_headers('Content-Type' = 'application/octet-stream') %>% 
                req_headers(
                    'Dropbox-API-Arg' = str_c(
                        '{"autorename":false,',
                        '"mode":"overwrite",',
                        str_glue('"path":"/R/lastfm/{treeFilename}",'),
                        '"strict_conflict":false}')
                ) %>% 
                req_body_file(path = here::here('tempData', treeFilename))
            
            respUpload <- req_perform(reqUpload)
        }
        
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
        
        reqUpload <- request('https://content.dropboxapi.com/2/files/upload/') %>% 
            req_oauth_refresh(client = dropboxClient, 
                              refresh_token = dropboxToken$refresh_token) %>% 
            req_headers('Content-Type' = 'application/octet-stream') %>% 
            req_headers(
                'Dropbox-API-Arg' = str_c('{',
                                          '"autorename":false,',
                                          '"mode":"overwrite",',
                                          '"path":"/R/lastfm/plexMasterRatings.csv",',
                                          '"strict_conflict":false', 
                                          '}')
            ) %>% 
            req_body_file(path = here::here('tempData', 'plexMasterRatings.csv'))
        
        respUpload <- req_perform(reqUpload)

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

plex <- getPlexRatings(T, T, T)

albumRatings %>%
    filter(x == y)
