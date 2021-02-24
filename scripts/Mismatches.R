## Check plex/lastfm mismatches

source('scripts/getLastfm.R')
source('scripts/getPlex.R')

lastfm <- getLastfm(T)
plex <- getPlex(T)
l1 <- lastfm %>% mutate_if(is.character, str_to_lower)
p1 <- plex %>% mutate_if(is.character, str_to_lower)

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
