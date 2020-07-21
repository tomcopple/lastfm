## Query Plex API?

getPlexAlbumRatings <- function(refresh = FALSE, write = FALSE) {
    library(tidyverse);library(httr);library(lubridate);library(rdrop2);library(treemapify)
    token <- '2CdDkLKF5xY27xxuxHB5'
    
    ## Music is section 5, need to search '10' to get all tracks
    if(refresh) {
        allTracks <- content(GET("http://192.168.1.99:32400/library/sections/5/search?type=10", 
                                 add_headers("X-Plex-Token" = token))) %>% 
            magrittr::extract2('MediaContainer') %>% 
            magrittr::extract2('Metadata')
        
        plex <- allTracks %>% 
            map_df(function(x) {
                as_tibble(x) %>% 
                    select(any_of(c("grandparentTitle", "originalTitle", "parentTitle", "title", "userRating")))
            }) %>% 
            select(albumArtist = grandparentTitle, artist = originalTitle, album = parentTitle, track = title, rating = userRating)
        
        treemap <- plex %>% 
            select(album, rating) %>% 
            mutate(rating = replace_na(rating, 0),
                   rating = round(rating / 2) * 2,
                   n = 1) %>% 
            mutate(rating = forcats::as_factor(rating)) %>% 
            ggplot(aes(area = n, fill = rating, subgroup = album)) +
                geom_treemap() +
                geom_treemap_subgroup_border(size = 1) +
                scale_fill_brewer()
        print(treemap)
        ggsave(plot = treemap,
               filename = here::here('tempData', str_c(lubridate::today(), '-treemap.png')),
               width = 10.5, height = 9.14, units = "in")
        rdrop2::drop_upload(file = here::here('tempData', str_c(lubridate::today(), '-treemap.png')),
                            path = 'R/lastfm/')
        
        
        ## Save a copy of albums/number of tracks?
        albumCount <- plex
            count(albumArtist, album)
        
        ## Save tracks that have a rating
        # rated <- allTracks %>% 
            # purrr::discard(function(x) is.null(x$userRating))
        
        ## And keep artist, album, title, ratings
        ratedDF <- plex %>% 
            filter(!is.na(rating))
        
    } else {
        ratedDF <- rdrop2::drop_read_csv('R/lastfm/plexMasterRatings.csv') %>% 
            as_tibble()
        
        
    }
    
    
    ## Save one copy date-stamped today, and another master copy
    if(write) {
        write_csv(ratedDF, here::here('tempData', 
                                      str_c(lubridate::today(), "-plexRatings.csv")))
        rdrop2::drop_upload(file = here::here('tempData', 
                                              str_c(lubridate::today(), "-plexRatings.csv")),
                            path = 'R/lastfm/')
        
        write_csv(ratedDF, here::here('tempData', 'plexMasterRatings.csv'))
        rdrop2::drop_upload(file = here::here('tempData', 'plexMasterRatings.csv'), 
                            path = 'R/lastfm/')
        
    }
        
    ratings <- ratedDF %>% group_by(albumArtist, album) %>% 
            filter(n() > 2) %>% 
            summarise(avRat = mean(rating)/2, 
                      n1 = n()) %>% 
            arrange(desc(avRat)) %>% 
            left_join(., albumCount) %>% 
            mutate(n = str_c(n1, "/", n)) %>% 
            select(-n1)
        print(ratings)
        .GlobalEnv$albumRatings <- ratings

    
    
    
    
    return(ratedDF)
}
