# lastfm

Ad hoc R scripts and a Shiny app for scraping, wrangling, and visualising Last.fm music data.

## Project layout

- `R/lib/`: reusable functions
- `R/analysis/`: one-off analysis scripts
- `apps/lastfm-shiny/`: Shiny dashboard app
- `reports/`: R Markdown dashboards
- `data/`: exports, cache, and archives

## Prerequisites

- R (4.1+ recommended)
- macOS users: Homebrew available (`brew`)

## Install

1. Clone the repo:

```bash
git clone https://github.com/tomcopple/lastfm.git
cd lastfm
```

2. Install system libraries (macOS/Homebrew):

```bash
brew install rust cmake udunits pkg-config harfbuzz fribidi abseil gdal geos proj fontconfig freetype libpng jpeg-turbo libtiff
```

3. Install R package manager helper:

```r
install.packages("remotes", repos = "https://cloud.r-project.org")
```

4. Install `dropboxr` from your GitHub repo:

```r
remotes::install_github("tomcopple/dropboxr")
```

5. Install project R dependencies:

```r
install.packages(c(
  "DT", "RColorBrewer", "flexdashboard", "forcats", "gganimate", "gifski",
  "glue", "here", "httr", "httr2", "jsonlite", "lubridate", "magrittr",
  "openssl", "plotly", "purrr", "rdrop2", "rvest", "scales", "shiny",
  "shinymaterial", "spotifyr", "stringr", "tidyverse", "treemapify",
  "xml2", "zoo"
), repos = "https://cloud.r-project.org")
```

## Configure secrets

The Shiny app expects project-level environment variables.

In `apps/lastfm-shiny`, create `.Renviron` (not committed) with:

```env
LASTFM_USER=...
LASTFM_APIKEY=...
DROPBOX_KEY=...
DROPBOX_SECRET=...
```

Then create a Dropbox auth cache once:

```r
dropboxr::dropbox_auth(Sys.getenv("DROPBOX_KEY"), Sys.getenv("DROPBOX_SECRET"), cache_path = "dropbox.rds")
```

## Run the Shiny app

From repo root:

```r
setwd("apps/lastfm-shiny")
shiny::runApp()
```

Or from terminal:

```bash
Rscript -e 'setwd("apps/lastfm-shiny"); shiny::runApp()'
```

## Notes

- Dropbox-backed files used by app workflows include `/R/lastfm/tracks.csv` and `/R/lastfm/plexDB.csv`.
- Root/project path helpers are in `R/bootstrap.R`.

## Discogs overrides

The dashboard and analysis scripts can apply manual matching overrides between Plex and Discogs albums.

- Primary file: `data/raw/discogs_overrides.tsv`
- Fallback file: `data/raw/discogs_overrides.csv` (used only if TSV is missing)

Expected columns:

- `plexArtist`
- `plexAlbum`
- `discogsArtist`
- `discogsAlbum`
- `notes` (optional free text)

Update workflow:

1. Edit `data/raw/discogs_overrides.tsv` and add one row per override mapping.
2. Keep artist/album names as plain text; matching slugs are generated automatically by the dashboard/script loaders.
3. In the Plex dashboard, open the "Discogs debug" section and click "Reload overrides".
4. Re-run any analysis script that consumes overrides (for example `R/analysis/2026-02-missingMajorWorks.R`) if you want exported outputs refreshed.

Example row:

```tsv
plexArtist	plexAlbum	discogsArtist	discogsAlbum	notes
Smog	A River Ain't Too Much to Love	Bill Callahan	A River Ain't Too Much to Love	Artist name changed on Discogs
```
