# Backup/restore script: fetches full Last.fm history and merges it

library(tidyverse)
library(lubridate)
library(httr2)
library(here)
library(glue)
library(dropboxr)

user <- Sys.getenv("LASTFM_USER")
api  <- Sys.getenv("LASTFM_APIKEY")

# Set to NULL to download full history
# stopDate <- ymd("2023-01-01")
stopDate <- NULL
use_stop_date <- !is.null(stopDate)
dropbox_tracks_path <- "/R/lastfm/tracks.csv"
base_url <- "http://ws.audioscrobbler.com/2.0/"

if (user == "" || api == "") stop("LASTFM_USER or LASTFM_APIKEY not set")

get_playcount <- function(user, api) {
    req <- request(base_url) %>%
        req_url_query(method = "user.getinfo", user = user, api_key = api, format = "json")

    resp <- tryCatch(req_perform(req), error = function(e) {
        stop(glue("Error fetching user info: {e$message}"))
    })

    if (resp_status(resp) >= 400) {
        body <- tryCatch(resp_body_string(resp), error = function(e) "<unreadable>")
        stop(glue("Failed to get user info (HTTP {resp_status(resp)}): {body}"))
    }

    json <- resp_body_json(resp, simplifyVector = TRUE)
    as.numeric(purrr::pluck(json, "user", "playcount", .default = 0))
}

fetch_page_tracks <- function(page, user, api) {
    req <- request(base_url) %>%
        req_url_query(
            method = "user.getrecenttracks",
            user = user,
            api_key = api,
            format = "json",
            limit = 200,
            page = page
        ) %>%
        req_retry(max_tries = 3)

    resp <- tryCatch(req_perform(req), error = function(e) {
        message(glue("Request failed for page {page}: {e$message}"))
        return(NULL)
    })

    if (is.null(resp)) return(tibble())
    if (resp_status(resp) >= 400) {
        body <- tryCatch(resp_body_string(resp), error = function(e) "<unreadable>")
        message(glue("Skipping page {page} (HTTP {resp_status(resp)}): {body}"))
        return(tibble())
    }

    parsed <- tryCatch(resp_body_json(resp, simplifyVector = TRUE), error = function(e) NULL)
    if (is.null(parsed)) return(tibble())

    tracks_raw <- purrr::pluck(parsed, "recenttracks", "track", .default = list())
    if (length(tracks_raw) == 0) return(tibble())

    as_tibble(tracks_raw) %>%
        transmute(
            track = name,
            artist = artist$`#text`,
            album = album$`#text`,
            date = parse_date_time(date$`#text`, orders = c("d b Y, H:M", "Y-m-d H:M:S"))
        ) %>%
        filter(!is.na(date))
}

playcount <- get_playcount(user, api)
pages <- if (playcount > 0) ceiling(playcount / 200) else 0
if (pages == 0) stop("No tracks found or failed to determine playcount")

message(glue("Fetching {playcount} plays across {pages} pages"))

results <- list()
for (i in seq_len(pages)) {
    message(glue("Fetching page {i}/{pages}"))
    page_tracks <- fetch_page_tracks(i, user, api)
    results[[length(results) + 1]] <- page_tracks

    if (use_stop_date && nrow(page_tracks) > 0 && min(page_tracks$date, na.rm = TRUE) <= stopDate) {
        message("Reached stop date; stopping early.")
        break
    }
}

restoreFinal <- bind_rows(results) %>%
    arrange(desc(date))

if (use_stop_date) {
    restoreFinal <- restoreFinal %>% filter(date >= stopDate)
}

restoreFinal <- restoreFinal %>% distinct()

if (nrow(restoreFinal) == 0) stop("No restored tracks to merge")

# Dropbox download/upload via dropboxr package
dropboxr::dropbox_auth()

lastfm <- tryCatch(
    dropboxr::download_dropbox_file(dropbox_tracks_path) %>%
        mutate(date = ymd_hms(date, quiet = TRUE)) %>%
        filter(!is.na(date)),
    error = function(e) stop(glue("Failed to download {dropbox_tracks_path}: {e$message}"))
)

newLastfm <- lastfm %>%
    filter(date < min(restoreFinal$date)) %>%
    bind_rows(restoreFinal) %>%
    arrange(date) %>%
    distinct()

dir.create(here::here("tempData"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("data", "exports"), recursive = TRUE, showWarnings = FALSE)

write_csv(newLastfm, here::here("tempData", str_c(today(), "-restoreLastfm.csv")))
write_csv(newLastfm, here::here("data", "exports", "tracks.csv"))

tryCatch(
    dropboxr::upload_df_to_dropbox(newLastfm, dropbox_path = dropbox_tracks_path,mode = 'overwrite'),
    error = function(e) stop(glue("Dropbox upload failed: {e$message}"))
)

message("Restore complete and uploaded to Dropbox.")
