## Build a low-cost "missing major works" list from existing exports.
##
## Output:
## data/exports/missingMajorWorks.csv

source(here::here("R", "bootstrap.R"))

library(tidyverse)
library(here)

min_album_scrobbles <- 8
min_artist_scrobbles <- 40
top_n_missing_by_owned_artist <- 250
top_n_new_artist_suggestions <- 100

normalise_text <- function(x) {
    x %>%
        str_to_lower() %>%
        str_replace_all("&", " and ") %>%
        str_replace_all("[[:punct:]]", " ") %>%
        str_replace_all("\\s+", " ") %>%
        str_trim()
}

album_key <- function(artist, album) {
    str_c(normalise_text(artist), normalise_text(album), sep = "|||")
}

tracks <- readr::read_csv(here("data", "exports", "tracks.csv"), show_col_types = FALSE)
if ('"track"' %in% names(tracks)) {
    tracks <- tracks %>% rename(track = `"track"`)
}

plex <- readr::read_csv(here("data", "exports", "plexDB.csv"), show_col_types = FALSE)

tracks_std <- tracks %>%
    transmute(
        artist = artist,
        album = album,
        date = suppressWarnings(lubridate::ymd_hms(date))
    ) %>%
    mutate(
        artist = normalise_text(artist),
        album = normalise_text(album),
        join_key = album_key(artist, album)
    ) %>%
    filter(artist != "", album != "")

plex_std <- plex %>%
    transmute(
        artist = coalesce(albumArtist, artist),
        album = album,
        rating = suppressWarnings(as.numeric(rating))
    ) %>%
    mutate(
        artist = normalise_text(artist),
        album = normalise_text(album),
        join_key = album_key(artist, album)
    ) %>%
    filter(artist != "", album != "")

overrides_path <- here("data", "raw", "discogs_overrides.tsv")
override_pairs <- if (file.exists(overrides_path)) {
    readr::read_tsv(overrides_path, show_col_types = FALSE) %>%
        transmute(
            plex_key = album_key(plexArtist, plexAlbum),
            discogs_key = album_key(discogsArtist, discogsAlbum)
        ) %>%
        filter(plex_key != "|||", discogs_key != "|||")
} else {
    tibble(plex_key = character(), discogs_key = character())
}

owned_album_keys <- plex_std %>% distinct(join_key) %>% pull(join_key)
owned_album_keys <- union(
    owned_album_keys,
    override_pairs %>%
        filter(plex_key %in% owned_album_keys) %>%
        pull(discogs_key)
)

owned_artists <- plex_std %>% distinct(artist) %>% pull(artist)

artist_listening <- tracks_std %>%
    count(artist, name = "artist_scrobbles")

artist_owned <- plex_std %>%
    group_by(artist) %>%
    summarise(
        owned_albums = n_distinct(album),
        avg_owned_rating = mean(rating, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(avg_owned_rating = ifelse(is.nan(avg_owned_rating), NA_real_, avg_owned_rating))

album_listening <- tracks_std %>%
    count(artist, album, join_key, name = "album_scrobbles")

missing_by_owned_artist <- album_listening %>%
    filter(
        artist %in% owned_artists,
        album_scrobbles >= min_album_scrobbles,
        !join_key %in% owned_album_keys
    ) %>%
    left_join(artist_listening, by = "artist") %>%
    left_join(artist_owned, by = "artist") %>%
    mutate(
        owned_albums = replace_na(owned_albums, 0),
        major_score =
            (2 * log1p(album_scrobbles)) +
            (0.8 * log1p(artist_scrobbles)) +
            (0.4 * pmin(owned_albums, 12)),
        recommendation_type = "missing_by_owned_artist",
        rationale = case_when(
            album_scrobbles >= 60 ~ "very_high_album_plays",
            album_scrobbles >= 30 ~ "high_album_plays",
            TRUE ~ "moderate_album_plays"
        )
    ) %>%
    arrange(desc(major_score), desc(album_scrobbles)) %>%
    slice_head(n = top_n_missing_by_owned_artist)

top_album_per_unowned_artist <- album_listening %>%
    filter(!artist %in% owned_artists, !join_key %in% owned_album_keys) %>%
    group_by(artist) %>%
    slice_max(order_by = album_scrobbles, n = 1, with_ties = FALSE) %>%
    ungroup()

new_artist_suggestions <- top_album_per_unowned_artist %>%
    left_join(artist_listening, by = "artist") %>%
    filter(artist_scrobbles >= min_artist_scrobbles) %>%
    mutate(
        major_score =
            (1.8 * log1p(album_scrobbles)) +
            (1.2 * log1p(artist_scrobbles)),
        recommendation_type = "new_artist_you_already_listen_to",
        rationale = case_when(
            artist_scrobbles >= 200 ~ "heavy_artist_listening",
            artist_scrobbles >= 100 ~ "strong_artist_listening",
            TRUE ~ "emerging_artist_listening"
        ),
        owned_albums = 0,
        avg_owned_rating = NA_real_
    ) %>%
    arrange(desc(major_score), desc(artist_scrobbles)) %>%
    slice_head(n = top_n_new_artist_suggestions)

recommendations <- bind_rows(
    missing_by_owned_artist,
    new_artist_suggestions
) %>%
    mutate(
        rank = row_number(desc(major_score)),
        artist_display = str_to_title(artist),
        album_display = str_to_title(album)
    ) %>%
    select(
        rank,
        recommendation_type,
        artist = artist_display,
        album = album_display,
        album_scrobbles,
        artist_scrobbles,
        owned_albums,
        avg_owned_rating,
        major_score,
        rationale,
        join_key
    )

output_path <- here("data", "exports", "missingMajorWorks.csv")
readr::write_csv(recommendations, output_path)

cat(str_glue(
    "Saved {nrow(recommendations)} recommendations to {output_path}\n",
    " - Missing by owned artists: {sum(recommendations$recommendation_type == 'missing_by_owned_artist')}\n",
    " - New artist suggestions: {sum(recommendations$recommendation_type == 'new_artist_you_already_listen_to')}\n"
))
