base_uri <- function() {
  "http://healthycanadians.gc.ca/recall-alert-rappel-avis/api"
}

parse_recent_recalls <- function(lang = "en") {
  glue::glue("{base_uri()}/recent/{lang}")
}
