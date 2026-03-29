## Get Discogs collection info

getDiscogs <- function(username = "tomcopple") {

    library(tidyverse);library(httr2);library(glue)

    slugger <- if (exists("getSlugs", mode = "function")) {
        getSlugs
    } else {
        function(text) {
            text %>%
                iconv(from = "UTF-8", to = "ASCII//TRANSLIT") %>%
                stringr::str_to_lower() %>%
                stringr::str_remove_all(pattern = "[[:punct:]]") %>%
                stringr::str_replace_all(pattern = "&", "and") %>%
                stringr::str_replace_all(pattern = "\\s{2,}", replacement = " ") %>%
                stringr::str_remove_all(pattern = "\\s\\(.*") %>%
                stringr::str_remove_all(pattern = "feat.*") %>%
                stringr::str_trim(side = "both") %>%
                stringr::str_replace_all(pattern = " ", replacement = "-")
        }
    }

    empty_collection <- tibble(
        rating = numeric(),
        id = numeric(),
        artist = character(),
        artistID = numeric(),
        album = character(),
        year = numeric(),
        format = character(),
        formatDesc = character(),
        genre = character(),
        match = character(),
        inDiscogs = logical()
    )

    discogs_token <- Sys.getenv("DISCOGS_TOKEN")
    if (!nzchar(discogs_token)) {
        warning("DISCOGS_TOKEN is not set; returning empty Discogs collection.")
        return(empty_collection)
    }

    base_url <- glue("https://api.discogs.com/users/{username}/collection/folders/0/releases")

    first_resp <- tryCatch(
        {
            request(base_url) %>%
                req_url_query(per_page = 500, page = 1) %>%
                req_headers(Authorization = glue("Discogs token={discogs_token}")) %>%
                req_perform() %>%
                resp_body_json()
        },
        error = function(e) NULL
    )

    if (is.null(first_resp)) {
        warning("Discogs API request failed; returning empty Discogs collection.")
        return(empty_collection)
    }

    total_pages <- first_resp %>%
        purrr::pluck("pagination", "pages", .default = 1)

    releases <- first_resp %>%
        purrr::pluck("releases", .default = list())

    if (total_pages > 1) {
        for (p in 2:total_pages) {
            page_releases <- tryCatch(
                {
                    request(base_url) %>%
                        req_url_query(per_page = 500, page = p) %>%
                        req_headers(Authorization = glue("Discogs token={discogs_token}")) %>%
                        req_perform() %>%
                        resp_body_json() %>%
                        purrr::pluck("releases", .default = list())
                },
                error = function(e) list()
            )

            releases <- c(releases, page_releases)
        }
    }

    if (length(releases) == 0) {
        return(empty_collection)
    }

    collection <- purrr::map_dfr(releases, function(x) {
        artist <- purrr::pluck(x, "basic_information", "artists", 1, "name", .default = NA_character_)
        artistID <- purrr::pluck(x, "basic_information", "artists", 1, "id", .default = NA_real_)
        album <- purrr::pluck(x, "basic_information", "title", .default = NA_character_)

        tibble(
            rating = purrr::pluck(x, "rating", .default = NA_real_),
            id = purrr::pluck(x, "basic_information", "id", .default = NA_real_),
            artist = artist,
            artistID = artistID,
            album = album,
            year = purrr::pluck(x, "basic_information", "year", .default = NA_real_),
            format = purrr::pluck(x, "basic_information", "formats", 1, "name", .default = NA_character_),
            formatDesc = purrr::pluck(x, "basic_information", "formats", 1, "descriptions", 1, .default = NA_character_),
            genre = purrr::pluck(x, "basic_information", "genres", 1, .default = NA_character_)
        )
    }) %>%
        filter(!is.na(.data$artist), !is.na(.data$album)) %>%
        mutate(
            match = slugger(str_c(.data$artist, .data$album, sep = "-")),
            inDiscogs = TRUE
        ) %>%
        distinct(.data$id, .keep_all = TRUE)

    return(collection)
}