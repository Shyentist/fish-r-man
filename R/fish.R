#' Send a query request for data
#'
#' @importFrom jsonlite fromJSON
#'
#' @description
#' Function to 'fish' for data using a 'bait' via fishRman's API.
#'
#' @param bait A 'bait' character scalar (endpoint) prepared via a bait function (e.g. `bait.gfw.effort()`).
#' @param sql Optional. A character scalar. Defaults to NULL. Select sql="count" to get the number of entries that the bait would normally return, sql="query" to get the query that will be run on the database.
#'
#' @returns The result of your query, usually a dataframe.
#'
#' @examples
#' \dontrun{
#' bait <- bait.gfw.effort(table = "fishing_effort_v2", min_lat = 0, flag = c("ITA", "FRA"))
#'
#' what_is_the_bait_selecting <- fish(bait = bait, sql = "query")
#'
#' how_many_rows_would_catch_have <- fish(bait = bait, sql = "count")
#'
#' catch <- fish(bait = bait)
#' }
#'
#' @seealso [bait.gfw.effort()]
#'
#' @export

fish <- function(bait, sql = NULL) {

  out <- tryCatch({
    if (missing(bait)) stop("You can only fish with a bait.")

    url <- "http://127.0.0.1:4000" # in the meantime

    if (!is.null(sql) && (sql == "query" || sql == "count")) {
      bait = paste(bait, "&sql=", sql, sep = "")
    }

    url <- paste(url, bait, sep = "")

    res <- fromJSON(url)

    return(res)
  },
  error=function(cond) {
    message("The resource you requested cannot be served. Error message:")
    message(cond)
    # Return value for an error
    return(NA)
  },
  warning=function(cond) {
    message("The resource you requested provided the following warning:")
    message(cond)
    # Return value for a warning
    return(NULL)
  })
}
