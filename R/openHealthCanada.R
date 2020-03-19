#' Recent Recalls
#'
#' @description Returns a response with the 15 latest recalls in each category,
#' all, food, vehicles, health, and cps.
#'
#'  @param lang Either \code{en} for english or \code{fr} for french, defaults
#'  to \code{en}.
#'  @param tibble Logical \code{TRUE} or \code{FALSE} to return a tibble,
#'  defaults to \code{TRUE}.
#'
#'  @example
#'  x <- recent_recalls()
#'  str(x, max.level = 1)
#'
#'  @export
recent_recalls <- function(lang = "en", tibble = TRUE) {
  request <- parse_recent_recalls(lang)
  results <- jsonlite::fromJSON(request)[["results"]]

  if (tibble)
    lapply(results, tibble::as_tibble)
  else
    results
}

#' Search Recalls
#'
#' @description Returns a list of 2; the recall results and the number of results found
#' based upon search parameters provided.
#'
#'  @param text The text string to search the database for
#'  @param lang Either \code{en} for english or \code{fr} for french, defaults
#'  to \code{en}.
#'  @param lim Limits the number of results. Defaults to 5 if not specified.
#'  @param cat Selects specific categories to search \code{1} for Food, \code{2}
#'  for Vehicles, \code{3} for Health Products, \code{4} for Consumer Products
#'  @param off Offsets the search results.
#'  @param tibble Logical \code{TRUE} or \code{FALSE} to return a tibble,
#'  defaults to \code{TRUE}.
#'
#'  @details
#'
#'  The number of results returned in the response is not fixed at 15; it can be changed using the lim parameter.
#'  The number of results found in the database is at the end of the response, as results_count.
#'  The department where the recall information came from is specified
#'
#'  @example
#'  x <- search_recalls(text="allergen",lim = "1",lang = "fr")
#'  str(x, max.level = 1)
#'
#'  @export

search_recalls <- function(text="",
                           lang="en",
                           lim="999",
                           cat="",
                           off="",
                           tibble = TRUE){

  request <- glue::glue("{base_uri()}/search?search={text}&lang={lang}&lim={lim}&cat={cat}&off={off}")
  results <- jsonlite::fromJSON(request)

  if (tibble)
    lapply(results, tibble::as_tibble)
  else
    results
}
