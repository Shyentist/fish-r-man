#' Send a query request for data
#'
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
#'
#' @description
#' Function to 'fish' for data using a 'bait' via fishRman's API.
#'
#' @param bait A 'bait' packaged via a bait function (e.g. `bait.gfw.effort()`)
#' @param JSON Logical. Defaults to FALSE. Whether the result should be in JSON format (text).
#'
#' @returns The result of your query, usually a dataframe.
#'
#' @examples
#' bait <- bait.gfw.effort(table = "fishing_effort_v2", min_lat = 0, flag = c("ITA", "FRA"))
#'
#' catch <- fish(bait = bait)
#'
#' @seealso [bait.gfw.effort()]
#'
#' @export

fish <- function(bait, JSON = FALSE) {

  if (missing(bait)) stop("You can only fish with a bait.")

  url <- "http://127.0.0.1:8000"

  endpoint <- bait[[1]]

  body <- bait[[2]]

  url <- paste(url, endpoint, sep = "")

  res <- httr::POST(
    url = url,
    body =  body,
    httr::add_headers(`accept` = 'application/json'),
    httr::content_type('application/json')
    )

  if (!JSON) {

    res <- jsonlite::fromJSON(content(res, as = "text"))

  }

}
