fixture_path <- function(...) {
  file.path("fixtures", "json", ...)
}

read_fixture_json <- function(name) {
  path <- fixture_path(name)
  jsonlite::fromJSON(path, simplifyDataFrame = FALSE)
}
