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
