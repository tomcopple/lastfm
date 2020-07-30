## Check plex tracks with no plays to identify mismatches

source('scripts/getLastfm.R')
source('scripts/getPlex.R')

lastfm <- getLastfm(refresh = FALSE)
plex <- getPlex(refresh = FALSE)

comb<- plex %>% 
    mutate(artist = ifelse(is.na(artist), albumArtist, artist)) %>% 
    left_join(lastfm %>% 
                  count(artist, track)) %>% 
    mutate(n = replace_na(n, 0))

comb %>% filter(n == 0) %>% 
    select(-n) %>% 
    count(artist, sort = T)

lastfm %>% filter(str_detect(artist, 'Belle')) %>% count(artist)
