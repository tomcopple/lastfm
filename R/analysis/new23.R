## New albums for 2023

library(tidyverse);library(lubridate)
source(here::here("R", "bootstrap.R"))
source_project("R", "lib", "getLastfm.R")

tracks <- getLastfm(T)

getNew <- function(enterYear = year(today())) {
    
    newTracks <- tracks %>% 
        mutate(year = year(date)) %>% 
        mutate(across(where(is.character), str_to_title)) %>% 
        group_by(artist, album) %>%
        count(track, year, name = 'trackPlays') %>%
        group_by(track) %>%
        top_n(n = -1, wt = year) %>%
        group_by(artist, album) %>% add_count(year, name = 'newTracks') %>%
        filter(year == enterYear, newTracks > 5) %>% 
        group_by(artist, album, newTracks) %>% 
        tally(trackPlays, name = 'albumPlays') %>% 
        ungroup() %>% 
        arrange(desc(albumPlays)) %>% na.omit()
    
    return(newTracks)
}

new <- getNew()
View(new)

## Get album year to filter out old records appearing for the first time
lastfm_api_key <- Sys.getenv('LASTFM_APIKEY')

library(httr2)
library(tidyverse)

get_album_year <- function(artist, album) {
    
    # Try Last.fm first
    year <- NA_character_
    
    year <- tryCatch({
        response <- request("http://ws.audioscrobbler.com/2.0/") |>
            req_url_query(
                method = "album.getinfo",
                api_key = lastfm_api_key,
                artist = artist,
                album = album,
                format = "json"
            ) |>
            req_perform() |>
            resp_body_json()
        
        # Try wiki content if we don't have a year yet
        if (is.na(year) && !is.null(response$album$wiki$content)) {
            content <- response$album$wiki$content
            # Search for 4-digit number starting with 1 or 2
            year_match <- regmatches(content, regexpr("[12]\\d{3}", content))
            if (length(year_match) > 0) {
                year <- year_match[1]
            }
        }
        
        # Try tags if we don't have a year yet
        if (is.na(year) && !is.null(response$album$tags) && !is.null(response$album$tags$tag)) {
            tag_list <- response$album$tags$tag
            if (!is.null(names(tag_list))) {
                tags <- tag_list$name
            } else {
                tags <- sapply(tag_list, function(x) x$name)
            }
            
            year_tags <- grep("^\\d{4}$", tags, value = TRUE)
            if (length(year_tags) > 0) {
                year <- year_tags[1]
            }
        }
        
        # Try releasedate if we still don't have a year
        if (is.na(year) && !is.null(response$album$releasedate)) {
            year <- substr(response$album$releasedate, 1, 4)
        }
        
        year
    }, error = function(e) {
        NA_character_
    })
    
    # If Last.fm didn't work, try MusicBrainz
    if (is.na(year)) {
        Sys.sleep(1)  # MusicBrainz requires rate limiting (1 request per second)
        
        year <- tryCatch({
            response <- request("https://musicbrainz.org/ws/2/release-group/") |>
                req_url_query(
                    query = paste0('artist:"', artist, '" AND releasegroup:"', album, '"'),
                    fmt = "json",
                    limit = 1
                ) |>
                req_user_agent("MyMusicApp/1.0 (tomcopple@gmail.com)") |>
                req_perform() |>
                resp_body_json()
            
            if (length(response$`release-groups`) > 0 && 
                !is.null(response$`release-groups`[[1]]$`first-release-date`)) {
                date_str <- response$`release-groups`[[1]]$`first-release-date`
                substr(date_str, 1, 4)
            } else {
                NA_character_
            }
        }, error = function(e) {
            NA_character_
        })
    }
    
    # If MusicBrainz didn't work, try Wikipedia
    if (is.na(year) || is.null(year) || length(year) == 0) {
        Sys.sleep(1)  # Be polite with rate limiting
        
        year <- tryCatch({
            # Search Wikipedia for the album
            search_query <- paste(album, artist, "album")
            response <- request("https://en.wikipedia.org/w/api.php") |>
                req_url_query(
                    action = "query",
                    list = "search",
                    srsearch = search_query,
                    format = "json",
                    srlimit = 1
                ) |>
                req_user_agent("MyMusicApp/1.0 (tomcopple@gmail.com)") |>
                req_perform() |>
                resp_body_json()
            
            # Get the snippet from the first search result
            if (length(response$query$search) > 0) {
                snippet <- response$query$search[[1]]$snippet
                # Remove HTML tags
                snippet <- gsub("<[^>]+>", "", snippet)
                # Search for 4-digit number starting with 1 or 2
                year_match <- regmatches(snippet, regexpr("[12]\\d{3}", snippet))
                if (length(year_match) > 0) {
                    year <- year_match[1]
                }
            }
            
            year
        }, error = function(e) {
            NA_character_
        })
    }
    
    return(year)
}

new <- new %>% mutate(year = map2_chr(artist, album, get_album_year))

get_album_year(artist = 'Labi Siffre', album = 'Remember My Song')
get_album_year(artist = 'Sault', album = 'Chapter 1')
get_album_year('The Science of Words', 'The Science of Words')
