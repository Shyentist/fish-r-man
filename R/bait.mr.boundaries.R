#' Prepare a file request for Marine Regions boundaries data
#'
#' @description
#' Preliminary function to prepare a 'bait' (API endpoint) to `fish()` for GFW fishing effort data via fishRman's API.
#'
#' @param file Character. The file you want to retrieve. Available files are 'eez_12nm', 'eez_24nm', and 'eez'.
#' @returns A character scalar (bait) to pass to `fish()`. The scalar contains the endpoint of the http GET request.
#'
#' @examples
#' \donttest{
#' bait <- bait.mr.boundaries(file = "eez")
#'
#' print(bait)
#'
#' # "/mr?file=eez_boundaries_v11"
#' }
#' @seealso [fish()]
#'
#' @export

bait.mr.boundaries <- function(file) {

  if (missing(file)) stop("You must specify a file")

  # prepare the endpoint of the http request
  if (file == "eez") {
    endpoint <- paste("/mr?file=", file,"_boundaries_v11", sep = "")
  } else if (file == "eez_12nm" || file == "eez_24nm"){
    endpoint <- paste("/mr?file=", file,"_v3_boundaries", sep = "")
  }

  # remove all whitespaces possibly introduced
  endpoint <- gsub(" ", "", endpoint)

  # return the bait
  return(endpoint)
}
