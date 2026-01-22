# Cache helper utilities

cache_path <- function(key) {
  file.path("cache", paste0(key, ".rds"))
}

get_cached <- function(key, fetch_fun) {
  path <- cache_path(key)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)

  if (file.exists(path)) {
    return(readRDS(path))
  }

  out <- fetch_fun()
  saveRDS(out, path)
  out
}
