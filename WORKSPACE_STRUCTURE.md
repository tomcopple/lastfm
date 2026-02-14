# Workspace structure

## Folders

- `R/lib/` reusable functions sourced by scripts and reports.
- `R/analysis/` one-off and exploratory analysis scripts.
- `apps/lastfm-shiny/` Shiny app.
- `reports/` R Markdown dashboards.
- `data/exports/` canonical CSV outputs used by scripts and app workflows.
- `data/cache/` generated cache artifacts.
- `data/archive/` dated backups.
- `.secrets/` local tokens and credentials files (gitignored).

## Compatibility notes

- `tempData/tracks.csv` remains symlinked to the canonical file in `data/exports/` for historical workflows.

## Bootstrap

- `R/bootstrap.R` defines `PROJ_ROOT` and `source_project(...)` for project-root-safe sourcing.
