## Refresh Last.fm data and save a local CSV copy.

source(here::here("R", "bootstrap.R"))
source_project("R", "lib", "getLastfm.R")

tracks <- getLastfm(refresh = TRUE)

output_path <- here::here("data", "exports", "tracks.csv")
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

readr::write_csv(tracks, output_path)

message(paste0("Saved ", nrow(tracks), " rows to ", output_path))
message(paste0("Latest play date: ", max(tracks$date, na.rm = TRUE)))
