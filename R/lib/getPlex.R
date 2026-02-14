## Get all Plex info

getPlex <- function(refresh = FALSE) {
    
    library(tidyverse);library(httr2);library(lubridate);library(jsonlite);library(httr2);library(xml2);library(dropboxr)
    dropboxr::dropbox_auth()
    
    
    if (refresh) {
        plexToken <- "8fx9FuiVxoZ6Pzvjtngg"
        
        plexRaw <- request("http://192.168.1.202:32400/library/sections/3/search?type=10") %>% 
            req_headers(`X-Plex-Token` = plexToken) %>% 
            req_perform() %>% 
            resp_body_xml()
        
        plex <- plexRaw %>% 
            xml2::xml_children() %>% 
            map_dfr(xml_attrs) %>% 
            as_tibble() %>% 
            select(
                albumArtist    = grandparentTitle, 
                   artist      = originalTitle, 
                   album       = parentTitle, 
                   track       = title, 
                   rating      = userRating,
                   key         = ratingKey,
                   artistKey   = grandparentRatingKey,
                   albumKey    = parentRatingKey,
                trackNum       = index,
                discNum        = parentIndex,
                year           = parentYear,
                duration       = duration
            ) %>% 
            mutate(artist = ifelse(is.na(artist), albumArtist, artist),
                   rating = as.numeric(rating),
                   trackNum = as.numeric(trackNum),
                   discNum = as.numeric(discNum),
                   year = as.numeric(year),
                   duration = as.numeric(duration)) %>% 
            as_tibble()
        
        print("Got Plex, uploading to Dropbox")
        dropboxr::upload_df_to_dropbox(plex, dropbox_path = "/R/lastfm/plexDB.csv")
        
    } else {
        
        plex <- dropboxr::download_dropbox_file("/R/lastfm/plexDB.csv")
        
    }
    
    return(plex)
}
