base_uri <- function() {
  "http://healthycanadians.gc.ca/recall-alert-rappel-avis/api"
}

parse_recent_recalls <- function(lang = "en") {
  glue::glue("{base_uri()}/recent/{lang}")
}

create.df <- function(.data,...){
  as.data.frame(matrix(.data,...))
}

parse_basic_details <- function(url){
  dts.html <- read_html(url)

  dts.nds.data <- html_nodes(dts.html,"dd.width45.paddingNone")
  dts.nds.colnames <- html_nodes(dts.html,"dt.paddingNone")

  dts.txt.data <- html_text(dts.nds.data)
  dts.txt.colnames <- html_text(dts.nds.colnames)

  output <- create.df(dts.txt.data,
                    ncol = length(dts.txt.data),
                    byrow = TRUE)
  names(output) <- dts.txt.colnames

  return(output)
}
