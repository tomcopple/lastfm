## Is it possible to download track ratings from RateYourMusic?
library(tidyverse);library(rvest);library(glue)

username <- 'tomcopple2'
rymURL <- glue::glue("https://rateyourmusic.com/collection/{username}/track_ratings")
rymProfile <- glue::glue("https://rateyourmusic.com/~{username}")

## NB can be multiple pages, need to find out how many albums first
numberOfAlbums <- rymProfile %>% 
    rvest::read_html() %>% 
    rvest::html_elements('#totaltrackratings') %>% 
    rvest::html_text() %>% 
    as.numeric()

## if numberOfAlbums > 25 then will be multiple pages
pageNum <- 0
allRatings <- tibble()

while (nrow(allRatings) < numberOfAlbums) {
    pageNum <- pageNum + 1
    
    pageUrl <- glue::glue("{rymURL}/{pageNum}")
    
    rymRaw <- rvest::read_html(pageUrl)
    
    ## Full table
    rymTab <- rymRaw %>% html_elements(".mbgen")
    
    
    # Get album ratings -------------------------------------------------------
    
    ## Separate out into individual album
    albumRatings <- rymTab %>% html_elements('.or_q_review') %>% html_elements('table')
    
    ## Tidy up each individual album and keep in list
    albumRatingsClean <- map(.x = albumRatings, .f = function(x) {
        x %>% html_elements('tr') %>% map_df(.f = function(x) {
            tibble(
                trackNum    = x %>% html_element('td') %>% html_text2(),
                trackName   = x %>% html_elements('td') %>% pluck(2) %>% html_text(),
                trackRating = x %>% html_element('img') %>% html_attr('title')
            ) %>% 
                mutate(trackRating = str_remove_all(trackRating, 'stars'),
                       trackRating = str_trim(trackRating),
                       trackRating = as.numeric(trackRating))
        })
    })
    
    
    # Get artist/album names --------------------------------------------------
    
    ## NB Sometimes albums have more than one artist, so can't just get all artist names from the page
    
    ## The album name is straightforward, it's here:
    # albumNames <- rymTab %>% html_elements('.album') %>% 
    #     html_text2()
    
    ## There's a artist/album/year summary here
    albumArtist <- rymTab %>% html_elements('.or_q_albumartist') %>% html_text2()
    
    ## Maybe just separate that out into artist, album, year
    artist <- str_extract(albumArtist, '^.*(?= - )')
    album <- str_extract(albumArtist, '(?<= - ).*(?= \\()')
    year <- str_extract(albumArtist, '(?<=\\()[0-9]{4}')
    
    
    # Put it all together -----------------------------------------------------
    
    allRatings <- bind_rows(allRatings, tibble(
        artist = artist,
        album = album,
        year = year,
        fullname = albumArtist,
        ratings = albumRatingsClean
    ))
    # allRatings %>% unnest(ratings)
} 

allRatings %>% mutate(avRat = map_dbl(ratings, ~mean(.$trackRating))) %>% arrange(desc(avRat))
