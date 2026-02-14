find_project_root <- function(start_dir = getwd()) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)

  repeat {
    if (file.exists(file.path(current_dir, "lastfm.Rproj"))) {
      return(current_dir)
    }

    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      stop("Could not locate project root containing lastfm.Rproj")
    }

    current_dir <- parent_dir
  }
}

if (!exists("PROJ_ROOT", inherits = FALSE)) {
  PROJ_ROOT <- find_project_root()
}

source_project <- function(...) {
  project_root <- if (exists("PROJ_ROOT", inherits = TRUE)) {
    get("PROJ_ROOT", inherits = TRUE)
  } else {
    find_project_root()
  }
  source(file.path(project_root, ...), local = parent.frame())
}
