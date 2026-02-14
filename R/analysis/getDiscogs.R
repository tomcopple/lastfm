## Discogs API?

library(tidyverse);library(httr2)

baseurl <- "https://api.discogs.com/"

# Get collection ---- 
discogs_key = Sys.getenv('DISCOGS_KEY')
discogs_secret = Sys.getenv('DISCOGS_SECRET')

discogs_client <- oauth_client(
        id = discogs_key,
        secret = discogs_secret,
        token_url = "https://api.discogs.com/oauth/request_token",
        name = "R_App"
    )

discogs_token <- "AbxUCQLUiAbrKnDZikoPMuPDYrnTljuLFpmJpVPh"
req <- request(str_glue("{baseurl}users/tomcopple/collection/folders/0/releases?per_page=500")) %>% 
    httr2::req_headers(Authorization = str_glue("Discogs token={discogs_token}"))
req_perform(request('https://api.discogs.com/oauth/identity') %>%
    httr2::req_headers(Authorization = str_glue("Discogs token={discogs_token}"))) %>% 
    resp_body_json()

resp <- req_perform(req)
raw <- resp %>% resp_body_json() %>% pluck('releases')
raw[[273]]$rating

## Information to keep?
## Artist, Album, OriginalYear, Genre
progress <- tibble(num = "", name = "")
df <- imap_dfr(raw, function(x, i) {
        progress <- rbind(progress, tibble(num = i, 
                                           name = x$basic_information$title))
        return <- tibble(
            temp = i, 
            rating = x$rating,
            id = x$basic_information$id,
            artist = x$basic_information$artists[[1]]$name,
            artistID = x$basic_information$artists[[1]]$id,
            album = x$basic_information$title,
            year = x$basic_information$year,
            format = x$basic_information$formats[[1]]$name,
            formatDesc = x$basic_information$formats[[1]]$descriptions[[1]],
            genre = x$basic_information$genres[[1]][[1]]
        )
        return(return)
    }) %>% 
    mutate(artist = str_remove_all(artist, " And The Athletes"),
           artist = str_remove_all(artist, " \\(.*"),
           artist = case_when(
               album == 'Madvillainy' ~ 'Madvillain',
               artist == 'Yesterdays New Quintet' ~ 'Madlib',
               str_detect(album, 'Hamilton: An American Musical') ~ 'Various Artists',
               album == 'Anatomy of a Murder' ~ 'Duke Ellington',
               .default = artist
           ),
           album = case_when(
               album == "Bon Iver, Bon Iver" ~ "Bon Iver",
               str_detect(album, " Otis Redding Sings Soul") ~ "Otis Blue: Otis Redding Sings Soul",
               str_detect(album, "Blues Brothers") ~ "The Blues Brothers",
               str_detect(album, "Chaleur Humaine") ~ "Chaleur Humaine",
               str_detect(album, "Depression Cherry") ~ "Depression Cherry",
               str_detect(album, "Dylan At Budokan") ~ "At Budokan",
               str_detect(album, "San Quentin") ~ "At San Quentin",
               str_detect(album, 'Rolled Gold') ~ "Rolled Gold",
               str_detect(album, 'Anatomy Of A Murder') ~ "Anatomy of a Murder",
               album == 'The Tired Sounds Of' ~ 'The Tired Sounds of Stars of the Lid',
               str_detect(album, 'Peanut Butter Blues') ~ 'Peanut Butter Blues and Melancholy Jam',
               .default = album
           ),
           album = str_trim(album)
    ) %>% 
    mutate(slug = str_trim(
        str_to_lower(
            str_replace_all(
                str_c(artist, album, sep = "-"),
                " ", "_"
            )
        )
    ))

df %>% count(artist, sort = T)

## Find a random album to listen to?
df %>% filter(rating == 0) %>% sample_n(1)

## Little treemap of album ratings so far
library(treemapify)
ggplot(df %>% 
           select(album, rating, genre) %>% 
           mutate(n = 1), 
       aes(area = n, fill = genre, subgroup = genre, alpha = rating)) + 
    geom_treemap() + 
    geom_treemap_subgroup_border(size = 5, aes(colour = genre, alpha = 1)) + 
    #scale_fill_brewer() + 
    theme(legend.position = 'none') + 
    geom_treemap_subgroup_text(size = 1)

library(plotly)
df %>% 
    distinct(genre) %>% 
    select(label = genre) %>% 
    mutate(parent = "", value = 0, rating = 0) %>% 
    bind_rows(
        df %>% 
            select(label = album, parent = genre, value = 1, rating)
    ) %>% 
    # mutate(rating) %>%
    plot_ly(
        type = 'treemap',
        labels = ~label, parents = ~parent, values = ~value,
        marker = list(colors = ~rating, opacity = ~rating)
    )
    

# Get lastfm ----
source(here::here("R", "bootstrap.R"))
source_project("R", "lib", "getLastfm.R")
lastfm <- getLastfm(T)

dfPlays <- df %>% 
    left_join(lastfm %>% 
                  mutate(artist = str_remove_all(artist, " and The Athletes")) %>% 
                  mutate(slug = str_c(artist, album, sep = "-")) %>% 
                  mutate(slug = str_to_lower(str_replace_all(slug, " ", "_"))) %>% 
                  count(slug),
              by = 'slug') %>% 
    mutate(n = replace_na(n, 0))

## Check missing plays
dfPlays %>% 
    filter(n == 0) %>% 
    select(temp, artist, album, slug)

## Most played albums not in collection
lastfm %>% 
    group_by(artist, album) %>% 
    count(sort = T) %>% 
    filter(n > 25) %>% 
    mutate(slug = str_c(artist, album, sep = "-")) %>% 
    mutate(slug = str_to_lower(str_replace_all(slug, " ", "_"))) %>% 
    left_join(df %>% 
                  mutate(slug = str_c(artist, album, sep = "-")) %>% 
                  mutate(slug = str_to_lower(str_replace_all(slug, " ", "_"))) %>% 
                  select(slug, id),
              by = 'slug') %>% 
    filter(is.na(id))

# Add Plex ----
source_project("R", "lib", "getPlex.R")
plex <- getPlex(T)

### Vinyl not in Plex
dfPlays %>% 
    left_join(
        plex %>% 
            mutate(slug = str_trim(
                str_to_lower(
                    str_replace_all(
                        str_c(albumArtist, album, sep = "-"),
                        " ", "_"
                    )
                )
            )) %>% 
            distinct(pArtist = albumArtist, 
                     pAlbum = album,
                     # album,
                     slug
                   ),
        by = 'slug'
    ) %>% 
    filter(is.na(pArtist)) %>% 
    distinct(artist, album, n, slug) %>% 
    arrange(desc(n))
    
## Highest rated albums not in discogs
plex %>% 
    group_by(albumArtist, album) %>% 
    filter(n() > 3) %>% 
    summarise(avRat = mean(rating)) %>% 
    ungroup() %>% 
    na.omit() %>% 
    arrange(desc(avRat)) %>% 
    mutate(slug = str_c(albumArtist, album, sep = "-")) %>% 
    mutate(slug = str_to_lower(str_replace_all(slug, " ", "_"))) %>% 
    left_join(df, by = 'slug') %>%
    select(albumArtist, album = album.x, avRat, slug, rating) %>% 
    head(20)
