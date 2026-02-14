# GitHub Copilot instructions for lastfmShiny

## Quick context ✅
- Single-file R Shiny app: `app.R` is the entrypoint and contains UI, server, and helper functions.
- Data sources lived in Dropbox: `/R/lastfm/tracks.csv` and `/R/lastfm/plexMasterRatings.csv` (downloaded via `dropboxr`).
- Deployed to shinyapps.io; deployment info in `rsconnect/shinyapps.io/tomcopple/lastfmShiny.dcf`.

## What an AI agent should know (high-value facts) 💡
- Environment variables in project `.Renviron` are required: `LASTFM_USER`, `LASTFM_APIKEY`, `DROPBOX_KEY`, `DROPBOX_SECRET`.
  - The app reads `.Renviron` at startup (`readRenviron(".Renviron")`).
  - NOTE: current `.Renviron` in repo contains secrets — do not leak or change them in commits.
- Dropbox auth token is cached in `dropbox.rds`; the code expects to read it as `dtoken <- readRDS('dropbox.rds')`.
  - To create/update it locally: `dropboxr::dropbox_auth(DROPBOX_KEY, DROPBOX_SECRET, cache_path = 'dropbox.rds')`.
- Fetching Last.fm scrobbles is done by `getLastfmShiny(refresh = TRUE)`. Behavior:
  - If `refresh = FALSE`: reads `tracks.csv` from Dropbox and returns local copy.
  - If `refresh = TRUE`: calls Last.fm API (via httr2-style `request()` + `req_perform()`), pages through results, appends new tracks to `tracks.csv` and uploads back to Dropbox.
- App UI inputs & outputs to reference in changes/debugging:
  - Inputs: `chooseType`, `chooseYear`, `chooseArtist`, `refresh` (button).
  - Outputs: `plotlyTime`, `plotlyBar`, `yearRatings`, `trackPlays`, `albumPlays`, `albumRatings`, `albumRatings2`.

## Important patterns & conventions 🔧
- All code lives in `app.R` (no modules). Helper functions (e.g., `getSlugs`, `removeEve`, `getLastfmShiny`) are defined near the top.
- Data transformations use `tidyverse` style (`dplyr`, `tidyr`, `stringr`, `forcats`). Plotting uses `plotly` and some `ggplot2`.
- External HTTP calls use httr2-style requests (`request()`, `req_retry`, `req_perform`, `resp_body_json`), so ensure `httr2` is installed if needed.
- Plex ratings are joined to plays by a slugification function `getSlugs()` (lowercase, punctuation removed, spaces → `-`). Use this when matching names.
- The app filters out some childrens' music in `removeEve()` (explicit list); keep this behavior when editing data flows.

## Build/Run/Debug workflows ▶️
- Run locally from R/RStudio: open `app.R` and click "Run App" or run `shiny::runApp()`; the file calls `shinyApp(ui, server)` at the end.
- To refresh data manually: click the app's "Refresh data" button (input `refresh`) or call `getLastfmShiny(TRUE)` in the R console.
- To update Dropbox credentials/token locally:
  - Set env vars using `usethis::edit_r_environ('project')` (or a local `.Renviron`) and then run `dropboxr::dropbox_auth(..., cache_path = 'dropbox.rds')`.
- Deploy: use `rsconnect::deployApp()` or the RStudio publish button; `rsconnect/` contains configuration for `shinyapps.io`.

## Security & housekeeping ⚠️
- Secrets (API keys, Dropbox tokens) are present in `.Renviron` and `dropbox.rds`. Avoid printing tokens in logs or committing new credentials.
- When adding tests or CI, mock HTTP/Dropbox calls (recorded fixtures or local CSVs) instead of using live credentials.

## Examples to copy when authoring changes ✍️
- Download tracks: `dropboxr::download_dropbox_file("/R/lastfm/tracks.csv", token = dtoken)`
- Upload updated tracks: `dropboxr::upload_df_to_dropbox(lastfmUpload, "/R/lastfm/tracks.csv", token = dtoken)`
- Slugify for matching: `getSlugs("The Beatles - Abbey Road") -> "the-beatles-abbey-road"`

## How to behave as an AI code agent 🤖
- Keep edits minimal and local: prefer adding helper functions over big refactors (this repo is single-file and sensitive to variable names/inputs).
- Preserve existing UI `inputId` and output names unless updating all references across `app.R`.
- When introducing new dependencies, add a short note in the repository root (e.g., `README.md`) and ensure they are cross-platform (macOS compatibility is needed).
- Do not add or check in secrets. If a change requires credentials, instruct the maintainer to provide them out-of-band.

---
If anything here is unclear or you'd like coverage of a particular area (tests, CI, packaging), tell me which part to expand and I will iterate. ✨