## Check plex/lastfm mismatches

source('scripts/getLastfm.R')
source('scripts/getPlex.R')

lastfm <- getLastfm(T)
plex <- getPlex(T)
l1 <- lastfm %>% mutate_if(is.character, str_to_lower) %>% 
    mutate_if(.predicate = is.character, .funs = function(x) str_remove_all(x, "[[:punct:]]"))
p1 <- plex %>% mutate_if(is.character, str_to_lower) %>% 
    mutate_if(.predicate = is.character, .funs = function(x) str_remove_all(x, "[[:punct:]]"))


# Get playcounts for Plex -------------------------------------------------

plays <- p1 %>% 
    left_join(l1 %>% count(artist, track))

# Plex tracks with no plays -----------------------------------------------

noPlex <- p1 %>% 
    left_join(l1 %>% 
                  count(artist, track)) %>% 
    filter(is.na(n))
noPlex %>% count(artist, sort = T)


# Most played lastfm with no Plex -----------------------------------------

l1 %>% count(artist, track) %>% 
    left_join(p1 %>% select(artist, track, album, albumArtist, rating, key)) %>% 
    filter(is.na(key)) %>% 
    arrange(desc(n))


# Lastfm missing albums ---------------------------------------------------

l2 <- l1 %>% filter(is.na(album))
l2 %>% count(artist, sort = T)
l2 %>% filter(str_detect(artist, 'belle.*sebastian')) %>% count(track, sort = T)


# Blank slates ------------------------------------------------------------

## Albums with no ratings, arranged by most plays
p1 %>% 
    group_by(albumArtist, album) %>% 
    add_count() %>% 
    filter(is.na(rating)) %>% 
    add_count() %>% 
    ungroup() %>% 
    select(artist = albumArtist, album, n, nn) %>% 
    filter(n == nn) %>% 
    distinct(artist, album) %>% 
    left_join(l1 %>% 
                  count(artist, album)) %>% 
    arrange(desc(n))


# Duplicate songs ---- 
## Make sure same songs are rated - NB check for live versions etc

## First check for any tracks with different ratings
p1 %>% 
    group_by(artist, track) %>% 
    filter(n() > 1) %>% 
    mutate(newRat = mean(rating, na.rm = TRUE)) %>% 
    filter(!is.na(newRat)) %>% 
    select(artist, albumArtist, album, track, rating, key, newRat) %>% 
    arrange(artist) %>% 
    filter(rating != newRat)

## Get duplicates where one is rated and one isn't
dups <- p1 %>% 
    group_by(artist, track) %>% 
    filter(n() > 1) %>% 
    mutate(newRat = mean(rating, na.rm = TRUE)) %>% 
    filter(!is.na(newRat)) %>% 
    filter(is.na(rating)) %>% 
    select(artist, albumArtist, album, track, rating, key, newRat) %>% 
    arrange(artist)

dups %>% select(artist, album, track, newRat)

## Copy this from restorePlexRatings.R
issues <- tibble(ratingKey = "", userRating = "")
sendRating <- function(ratingKey, userRating) {
    req <- httr::GET("http://192.168.1.99:32400/:/rate", 
                     query = list(
                         "X-Plex-Token" = token,
                         "identifier"   = "com.plexapp.plugins.library",
                         "key"          = ratingKey,
                         "rating"       = as.character(userRating)))
    if(status_code(req) != 200) {
        print(str_c("Problem with ", ratingKey))
        .GlobalEnv$issues <- rbind(issues, ratingKey = ratingKey, userRating = userRating)
    } else if (status_code(req) == 200) {
        print(str_c(ratingKey, " successful"))
    }
    
}

purrr::walk2(.x = dups$key, .y = dups$newRat, .f = sendRating)
