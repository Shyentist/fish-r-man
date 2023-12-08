#' Send a query request for data
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET content timeout status_code
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
#' \donttest{
#' bait <- bait.gfw.effort(table = "fishing_effort_byvessel_v2", end_date="2012-01-02")
#'
#' what_is_the_bait_selecting <- fish(bait = bait, sql = "query")
#'
#' print(what_is_the_bait_selecting)
#' # "SELECT * FROM gfw_fishing_effort_byvessel_v2 WHERE dated <= '2012-01-02'"
#' }
#'
#' @seealso [bait.gfw.effort()]
#'
#' @export

fish <- function(bait, sql = NULL) {

  out <- tryCatch({
    if (missing(bait)) stop("You can only fish with a bait.")

    url <- "https://fishrman.ddnsfree.com" # in the meantime

    if (!is.null(sql) && (sql == "query" || sql == "count")) {
      bait = paste(bait, "&sql=", sql, sep = "")
    }

    url <- paste(url, bait, sep = "")

    res <- GET(url, timeout(300))
    # Check the status code of the response
    status <- status_code(res)

    # Print or process the response based on the status code
    if (status == 200) {
      # Successful response
      res <- content(x = res, as = "text", encoding = "UTF-8")
      # Process the content as needed
    } else {
      # Handle other status codes or errors
      stop(paste("Error: Status Code", status))
    }

    # files from /mr are kept as geojson, the others go from JSON to R object
    if (substr(bait, 1, 3) != "/mr") {
      res <- fromJSON(res)
    }

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
