## Build AI-backed recommendations for albums missing from Discogs collection.
##
## Outputs:
## data/exports/missingDiscogsCandidates.csv
## data/exports/missingDiscogsAISuggestions.csv

source(here::here("R", "bootstrap.R"))

library(tidyverse)
library(here)
library(httr2)
library(jsonlite)

min_album_scrobbles <- 8
min_artist_scrobbles <- 40
candidate_pool_size <- 350
ai_max_suggestions <- 100

normalise_text <- function(x) {
    x %>%
        str_to_lower() %>%
        str_replace_all("&", " and ") %>%
        str_replace_all("[[:punct:]]", " ") %>%
        str_replace_all("\\s+", " ") %>%
        str_trim()
}

slug_key <- function(artist, album) {
    str_c(normalise_text(artist), normalise_text(album), sep = "|||")
}

get_discogs_collection <- function(username = "tomcopple") {
    discogs_token <- Sys.getenv("DISCOGS_TOKEN")

    if (!nzchar(discogs_token)) {
        return(tibble::tibble(artist = character(), album = character(), join_key = character()))
    }

    req <- httr2::request(glue::glue("https://api.discogs.com/users/{username}/collection/folders/0/releases?per_page=500")) %>%
        httr2::req_headers(Authorization = glue::glue("Discogs token={discogs_token}"))

    releases <- tryCatch(
        {
            httr2::req_perform(req) %>%
                httr2::resp_body_json() %>%
                purrr::pluck("releases")
        },
        error = function(e) list()
    )

    if (length(releases) == 0) {
        return(tibble::tibble(artist = character(), album = character(), join_key = character()))
    }

    purrr::map_dfr(releases, function(x) {
        tibble::tibble(
            artist = purrr::pluck(x, "basic_information", "artists", 1, "name", .default = NA_character_),
            album = purrr::pluck(x, "basic_information", "title", .default = NA_character_)
        )
    }) %>%
        dplyr::filter(!is.na(.data$artist), !is.na(.data$album)) %>%
        dplyr::mutate(join_key = slug_key(.data$artist, .data$album)) %>%
        dplyr::distinct(.data$join_key, .keep_all = TRUE)
}

load_overrides <- function(path = here("data", "raw", "discogs_overrides.tsv")) {
    if (!file.exists(path)) {
        return(tibble(plex_key = character(), discogs_key = character()))
    }

    readr::read_tsv(path, show_col_types = FALSE) %>%
        transmute(
            plex_key = slug_key(plexArtist, plexAlbum),
            discogs_key = slug_key(discogsArtist, discogsAlbum)
        ) %>%
        filter(.data$plex_key != "|||", .data$discogs_key != "|||") %>%
        distinct()
}

call_openai_for_recommendations <- function(existing_collection_data, max_suggestions = 100) {
    debug_path <- here("data", "exports", "missingDiscogsOpenAIDebug.json")

    write_debug <- function(payload) {
        jsonlite::write_json(payload, debug_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    }

    openai_key <- Sys.getenv("OPENAI_API_KEY")
    openai_model <- Sys.getenv("OPENAI_MODEL", unset = "gpt-4o-mini")

    if (!nzchar(openai_key)) {
        write_debug(list(
            timestamp = as.character(Sys.time()),
            source = "none",
            model = openai_model,
            reason = "OPENAI_API_KEY not set"
        ))
        return(list(data = NULL, source = "heuristic_fallback", reason = "OPENAI_API_KEY not set", model = openai_model))
    }

    if (nrow(existing_collection_data) == 0) {
        write_debug(list(
            timestamp = as.character(Sys.time()),
            source = "none",
            model = openai_model,
            reason = "No candidate rows available"
        ))
        return(list(data = NULL, source = "heuristic_fallback", reason = "No Discogs collection rows available", model = openai_model))
    }

    prompt_payload <- existing_collection_data %>%
        dplyr::select(.data$artist, .data$album) %>%
        dplyr::distinct() %>%
        dplyr::arrange(.data$artist, .data$album) %>%
        jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

    system_prompt <- paste(
        "You are a music collection recommendation agent.",
        "Task: suggest obvious major albums missing from the user's Discogs collection.",
        "Use only artists already present in the provided Discogs collection list.",
        "Do not suggest albums already present in that list.",
        "Return strict JSON with top-level key 'suggestions' as an array.",
        "Each suggestion object must include: artist, album, why, confidence_0_100.",
        "Keep why short and specific."
    )

    user_prompt <- str_glue(
        "Current Discogs collection rows:\n{prompt_payload}\n\n",
        "Return at most {max_suggestions} suggestions sorted by best first."
    )

    req_body <- list(
        model = openai_model,
        input = list(
            list(role = "system", content = system_prompt),
            list(role = "user", content = user_prompt)
        )
    )

    api_response <- tryCatch(
        {
            request("https://api.openai.com/v1/responses") %>%
                req_headers(
                    Authorization = paste("Bearer", openai_key),
                    `Content-Type` = "application/json"
                ) %>%
                req_body_json(req_body, auto_unbox = TRUE) %>%
                req_error(is_error = function(resp) FALSE) %>%
                req_perform()
        },
        error = function(e) e
    )

    if (inherits(api_response, "error")) {
        write_debug(list(
            timestamp = as.character(Sys.time()),
            source = "responses",
            model = openai_model,
            request_body = req_body,
            reason = str_glue("Request error: {api_response$message}")
        ))
        return(list(
            data = NULL,
            source = "heuristic_fallback",
            reason = str_glue("Request error: {api_response$message}"),
            model = openai_model
        ))
    }

    status <- resp_status(api_response)
    responses_body_text <- tryCatch(resp_body_string(api_response), error = function(e) NA_character_)
    response <- tryCatch(resp_body_json(api_response, simplifyVector = FALSE), error = function(e) NULL)

    if (is.null(response)) {
        write_debug(list(
            timestamp = as.character(Sys.time()),
            source = "responses",
            model = openai_model,
            request_body = req_body,
            http_status = status,
            raw_body = responses_body_text,
            reason = str_glue("OpenAI response could not be parsed (HTTP {status})")
        ))
        return(list(
            data = NULL,
            source = "heuristic_fallback",
            reason = str_glue("OpenAI response could not be parsed (HTTP {status})"),
            model = openai_model
        ))
    }

    if (status >= 300) {
        api_error_message <- tryCatch(response$error$message, error = function(e) NA_character_)
        if (is.na(api_error_message) || !nzchar(api_error_message)) {
            api_error_message <- str_glue("HTTP {status}")
        }
        write_debug(list(
            timestamp = as.character(Sys.time()),
            source = "responses",
            model = openai_model,
            request_body = req_body,
            http_status = status,
            response_body = response,
            reason = str_glue("OpenAI API error: {api_error_message}")
        ))
        return(list(
            data = NULL,
            source = "heuristic_fallback",
            reason = str_glue("OpenAI API error: {api_error_message}"),
            model = openai_model
        ))
    }

    output_text <- tryCatch(
        {
            if (!is.null(response$output_text) && nzchar(response$output_text)) {
                return(response$output_text)
            }

            output_items <- response$output
            if (is.null(output_items) || length(output_items) == 0) {
                return(NA_character_)
            }

            text_items <- purrr::map(output_items, function(item) {
                content_items <- item$content
                if (is.null(content_items) || length(content_items) == 0) {
                    return(character())
                }

                purrr::map_chr(content_items, ~ {
                    if (is.null(.x$text)) "" else as.character(.x$text)
                })
            }) %>%
                unlist(use.names = FALSE)

            text_items <- text_items[nzchar(text_items)]

            if (length(text_items) == 0) NA_character_ else text_items[[1]]
        },
        error = function(e) NA_character_
    )

    if (is.na(output_text) || !nzchar(output_text)) {
        chat_body <- list(
            model = openai_model,
            messages = list(
                list(role = "system", content = system_prompt),
                list(role = "user", content = user_prompt)
            ),
            response_format = list(type = "json_object")
        )

        chat_response <- tryCatch(
            {
                request("https://api.openai.com/v1/chat/completions") %>%
                    req_headers(
                        Authorization = paste("Bearer", openai_key),
                        `Content-Type` = "application/json"
                    ) %>%
                    req_body_json(chat_body, auto_unbox = TRUE) %>%
                    req_error(is_error = function(resp) FALSE) %>%
                    req_perform()
            },
            error = function(e) e
        )

        if (inherits(chat_response, "error")) {
            write_debug(list(
                timestamp = as.character(Sys.time()),
                source = "chat_completions",
                model = openai_model,
                responses_request_body = req_body,
                responses_http_status = status,
                responses_body = response,
                chat_request_body = chat_body,
                reason = str_glue("Responses had no text; chat fallback request error: {chat_response$message}")
            ))
            return(list(
                data = NULL,
                source = "heuristic_fallback",
                reason = str_glue("Responses had no text; chat fallback request error: {chat_response$message}"),
                model = openai_model
            ))
        }

        chat_status <- resp_status(chat_response)
        chat_body_text <- tryCatch(resp_body_string(chat_response), error = function(e) NA_character_)
        chat_body_json <- tryCatch(resp_body_json(chat_response, simplifyVector = FALSE), error = function(e) NULL)

        if (is.null(chat_body_json)) {
            write_debug(list(
                timestamp = as.character(Sys.time()),
                source = "chat_completions",
                model = openai_model,
                responses_request_body = req_body,
                responses_http_status = status,
                responses_body = response,
                chat_request_body = chat_body,
                chat_http_status = chat_status,
                chat_raw_body = chat_body_text,
                reason = str_glue("Responses had no text; chat fallback response parse failed (HTTP {chat_status})")
            ))
            return(list(
                data = NULL,
                source = "heuristic_fallback",
                reason = str_glue("Responses had no text; chat fallback response parse failed (HTTP {chat_status})"),
                model = openai_model
            ))
        }

        if (chat_status >= 300) {
            api_error_message <- tryCatch(chat_body_json$error$message, error = function(e) NA_character_)
            if (is.na(api_error_message) || !nzchar(api_error_message)) {
                api_error_message <- str_glue("HTTP {chat_status}")
            }
            write_debug(list(
                timestamp = as.character(Sys.time()),
                source = "chat_completions",
                model = openai_model,
                responses_request_body = req_body,
                responses_http_status = status,
                responses_body = response,
                chat_request_body = chat_body,
                chat_http_status = chat_status,
                chat_response_body = chat_body_json,
                reason = str_glue("Responses had no text; chat fallback API error: {api_error_message}")
            ))
            return(list(
                data = NULL,
                source = "heuristic_fallback",
                reason = str_glue("Responses had no text; chat fallback API error: {api_error_message}"),
                model = openai_model
            ))
        }

        output_text <- tryCatch(
            {
                choices <- chat_body_json$choices
                if (is.null(choices) || length(choices) == 0) {
                    return(NA_character_)
                }

                content <- choices[[1]]$message$content
                if (is.null(content)) {
                    return(NA_character_)
                }

                if (is.character(content) && length(content) > 0) {
                    return(content[[1]])
                }

                if (is.list(content)) {
                    text_items <- purrr::map_chr(content, ~ {
                        if (!is.null(.x$text)) as.character(.x$text) else ""
                    })
                    text_items <- text_items[nzchar(text_items)]
                    if (length(text_items) > 0) {
                        return(text_items[[1]])
                    }
                }

                NA_character_
            },
            error = function(e) NA_character_
        )

        if (is.na(output_text) || !nzchar(output_text)) {
            write_debug(list(
                timestamp = as.character(Sys.time()),
                source = "chat_completions",
                model = openai_model,
                responses_request_body = req_body,
                responses_http_status = status,
                responses_body = response,
                chat_request_body = chat_body,
                chat_http_status = chat_status,
                chat_response_body = chat_body_json,
                reason = "OpenAI returned no text output from both Responses and chat fallback"
            ))
            return(list(
                data = NULL,
                source = "heuristic_fallback",
                reason = "OpenAI returned no text output from both Responses and chat fallback",
                model = openai_model
            ))
        }
    }

    json_text <- output_text
    parsed <- tryCatch(jsonlite::fromJSON(json_text), error = function(e) NULL)

    if (is.null(parsed)) {
        json_block <- stringr::str_extract(output_text, "\\{[\\s\\S]*\\}")
        parsed <- tryCatch(jsonlite::fromJSON(json_block), error = function(e) NULL)
    }

    if (is.null(parsed) || is.null(parsed$suggestions)) {
        write_debug(list(
            timestamp = as.character(Sys.time()),
            source = "responses_or_chat",
            model = openai_model,
            output_text_preview = stringr::str_sub(output_text, 1, 5000),
            reason = "OpenAI output did not include a valid suggestions JSON payload"
        ))
        return(list(
            data = NULL,
            source = "heuristic_fallback",
            reason = "OpenAI output did not include a valid suggestions JSON payload",
            model = openai_model
        ))
    }

    suggestion_data <- as_tibble(parsed$suggestions) %>%
        transmute(
            artist = as.character(.data$artist),
            album = as.character(.data$album),
            ai_why = as.character(.data$why),
            ai_confidence = suppressWarnings(as.numeric(.data$confidence_0_100))
        ) %>%
        filter(!is.na(.data$artist), !is.na(.data$album), .data$artist != "", .data$album != "") %>%
        distinct(.data$artist, .data$album, .keep_all = TRUE)

    if (nrow(suggestion_data) == 0) {
        write_debug(list(
            timestamp = as.character(Sys.time()),
            source = "responses_or_chat",
            model = openai_model,
            output_text_preview = stringr::str_sub(output_text, 1, 5000),
            parsed = parsed,
            reason = "OpenAI returned suggestions, but none were usable"
        ))
        return(list(
            data = NULL,
            source = "heuristic_fallback",
            reason = "OpenAI returned suggestions, but none were usable",
            model = openai_model
        ))
    }

    write_debug(list(
        timestamp = as.character(Sys.time()),
        source = "openai",
        model = openai_model,
        suggestion_count = nrow(suggestion_data),
        reason = "OK"
    ))

    list(data = suggestion_data, source = "openai", reason = "OK", model = openai_model)
}

tracks <- readr::read_csv(here("data", "exports", "tracks.csv"), show_col_types = FALSE)
if ('"track"' %in% names(tracks)) {
    tracks <- tracks %>% rename(track = `"track"`)
}

plex <- readr::read_csv(here("data", "exports", "plexDB.csv"), show_col_types = FALSE)
overrides <- load_overrides()
discogs_collection <- get_discogs_collection("tomcopple")

discogs_collection_for_ai <- discogs_collection %>%
    transmute(artist = .data$artist, album = .data$album) %>%
    distinct()

tracks_std <- tracks %>%
    transmute(artist = artist, album = album) %>%
    mutate(
        artist_norm = normalise_text(.data$artist),
        album_norm = normalise_text(.data$album),
        join_key = slug_key(.data$artist_norm, .data$album_norm)
    ) %>%
    filter(.data$artist_norm != "", .data$album_norm != "")

plex_std <- plex %>%
    transmute(
        artist = coalesce(albumArtist, artist),
        album = album,
        rating = suppressWarnings(as.numeric(rating))
    ) %>%
    mutate(
        artist_norm = normalise_text(.data$artist),
        album_norm = normalise_text(.data$album),
        join_key = slug_key(.data$artist_norm, .data$album_norm)
    ) %>%
    filter(.data$artist_norm != "", .data$album_norm != "")

discogs_keys <- discogs_collection %>%
    transmute(join_key = .data$join_key) %>%
    distinct()

discogs_keys <- bind_rows(
    discogs_keys,
    overrides %>% transmute(join_key = .data$discogs_key)
) %>% distinct()

artist_scrobbles <- tracks_std %>%
    count(.data$artist_norm, name = "artist_scrobbles")

album_scrobbles <- tracks_std %>%
    count(.data$artist_norm, .data$album_norm, .data$join_key, name = "album_scrobbles")

plex_artist_stats <- plex_std %>%
    group_by(.data$artist_norm) %>%
    summarise(
        owned_plex_albums_by_artist = n_distinct(.data$album_norm),
        avg_plex_rating_for_artist = mean(.data$rating, na.rm = TRUE),
        .groups = "drop"
    ) %>%
    mutate(avg_plex_rating_for_artist = ifelse(is.nan(.data$avg_plex_rating_for_artist), NA_real_, .data$avg_plex_rating_for_artist))

all_missing_signals <- album_scrobbles %>%
    left_join(artist_scrobbles, by = "artist_norm") %>%
    left_join(plex_artist_stats, by = "artist_norm") %>%
    mutate(
        owned_plex_albums_by_artist = replace_na(.data$owned_plex_albums_by_artist, 0L),
        baseline_score =
            (2.1 * log1p(.data$album_scrobbles)) +
            (0.9 * log1p(.data$artist_scrobbles)) +
            (0.45 * pmin(.data$owned_plex_albums_by_artist, 12)),
        baseline_reason = case_when(
            .data$album_scrobbles >= 80 ~ "very_high_album_plays",
            .data$album_scrobbles >= 40 ~ "high_album_plays",
            .data$artist_scrobbles >= 200 ~ "high_artist_listening",
            TRUE ~ "consistent_listening_signal"
        )
    ) %>%
    anti_join(discogs_keys, by = "join_key") %>%
    mutate(
        artist = str_to_title(.data$artist_norm),
        album = str_to_title(.data$album_norm)
    ) %>%
    select(
        artist,
        album,
        artist_norm,
        album_norm,
        join_key,
        album_scrobbles,
        artist_scrobbles,
        owned_plex_albums_by_artist,
        avg_plex_rating_for_artist,
        baseline_score,
        baseline_reason
    )

candidates <- album_scrobbles %>%
    left_join(artist_scrobbles, by = "artist_norm") %>%
    left_join(plex_artist_stats, by = "artist_norm") %>%
    mutate(
        owned_plex_albums_by_artist = replace_na(.data$owned_plex_albums_by_artist, 0L),
        baseline_score =
            (2.1 * log1p(.data$album_scrobbles)) +
            (0.9 * log1p(.data$artist_scrobbles)) +
            (0.45 * pmin(.data$owned_plex_albums_by_artist, 12)),
        baseline_reason = case_when(
            .data$album_scrobbles >= 80 ~ "very_high_album_plays",
            .data$album_scrobbles >= 40 ~ "high_album_plays",
            .data$artist_scrobbles >= 200 ~ "high_artist_listening",
            TRUE ~ "consistent_listening_signal"
        )
    ) %>%
    filter(
        .data$album_scrobbles >= min_album_scrobbles,
        .data$artist_scrobbles >= min_artist_scrobbles
    ) %>%
    anti_join(discogs_keys, by = "join_key") %>%
    arrange(desc(.data$baseline_score), desc(.data$album_scrobbles)) %>%
    slice_head(n = candidate_pool_size) %>%
    mutate(
        artist = str_to_title(.data$artist_norm),
        album = str_to_title(.data$album_norm)
    ) %>%
    select(
        artist,
        album,
        album_scrobbles,
        artist_scrobbles,
        owned_plex_albums_by_artist,
        avg_plex_rating_for_artist,
        baseline_score,
        baseline_reason,
        join_key
    )

candidates_path <- here("data", "exports", "missingDiscogsCandidates.csv")
readr::write_csv(candidates, candidates_path)

ai_result <- call_openai_for_recommendations(discogs_collection_for_ai, ai_max_suggestions)
ai_suggestions <- ai_result$data

openai_ranked <- if (!is.null(ai_suggestions) && nrow(ai_suggestions) > 0) {
    ai_suggestions %>%
        mutate(
            artist_key = normalise_text(.data$artist),
            album_key = normalise_text(.data$album)
        ) %>%
        left_join(
            all_missing_signals %>%
                mutate(
                    artist_key = normalise_text(.data$artist),
                    album_key = normalise_text(.data$album)
                ),
            by = c("artist_key", "album_key")
        ) %>%
        mutate(
            album_scrobbles = replace_na(.data$album_scrobbles, 0L),
            artist_scrobbles = replace_na(.data$artist_scrobbles, 0L),
            owned_plex_albums_by_artist = replace_na(.data$owned_plex_albums_by_artist, 0L),
            baseline_score = replace_na(.data$baseline_score, 0),
            baseline_reason = replace_na(.data$baseline_reason, "ai_major_work"),
            join_key = dplyr::if_else(
                is.na(.data$join_key),
                slug_key(.data$artist.x, .data$album.x),
                .data$join_key
            )
        ) %>%
        anti_join(discogs_keys, by = "join_key") %>%
        arrange(desc(.data$ai_confidence), desc(.data$baseline_score)) %>%
        mutate(
            recommendation_source = "openai",
            rank = row_number()
        ) %>%
        select(
            rank,
            recommendation_source,
            artist = .data$artist.x,
            album = .data$album.x,
            ai_why,
            ai_confidence,
            album_scrobbles,
            artist_scrobbles,
            owned_plex_albums_by_artist,
            avg_plex_rating_for_artist,
            baseline_score,
            baseline_reason,
            join_key
        )
} else {
    tibble()
}

if (nrow(openai_ranked) == 0 && identical(ai_result$source, "openai") && !identical(ai_result$reason, "OK")) {
    ai_result$reason <- str_c(ai_result$reason)
}

if (nrow(openai_ranked) == 0 && identical(ai_result$source, "openai") && identical(ai_result$reason, "OK")) {
    ai_result$reason <- "OpenAI returned suggestions but none matched candidate albums after normalization"
}

final_suggestions <- if (nrow(openai_ranked) > 0) {
    openai_ranked
} else {
    candidates %>%
        arrange(desc(.data$baseline_score), desc(.data$album_scrobbles)) %>%
        slice_head(n = ai_max_suggestions) %>%
        mutate(
            rank = row_number(),
            recommendation_source = "heuristic_fallback",
            ai_why = str_replace_all(.data$baseline_reason, "_", " "),
            ai_confidence = round(pmin(98, 55 + (7 * log1p(.data$album_scrobbles)) + (3 * log1p(.data$artist_scrobbles))), 0)
        ) %>%
        select(
            .data$rank,
            .data$recommendation_source,
            .data$artist,
            .data$album,
            .data$ai_why,
            .data$ai_confidence,
            .data$album_scrobbles,
            .data$artist_scrobbles,
            .data$owned_plex_albums_by_artist,
            .data$avg_plex_rating_for_artist,
            .data$baseline_score,
            .data$baseline_reason,
            .data$join_key
        )
}

final_path <- here("data", "exports", "missingDiscogsAISuggestions.csv")
readr::write_csv(final_suggestions, final_path)

cat(str_glue(
    "Saved {nrow(candidates)} Discogs-missing candidates to {candidates_path}\n",
    "Saved {nrow(final_suggestions)} AI suggestions to {final_path}\n",
    "OpenAI debug: {here('data', 'exports', 'missingDiscogsOpenAIDebug.json')}\n",
    "Source: {ifelse(any(final_suggestions$recommendation_source == 'openai'), 'openai', 'heuristic_fallback')}\n",
    "Key present: {ifelse(nzchar(Sys.getenv('OPENAI_API_KEY')), 'yes', 'no')}\n",
    "Model: {ai_result$model}\n",
    "Reason: {ai_result$reason}\n"
))
