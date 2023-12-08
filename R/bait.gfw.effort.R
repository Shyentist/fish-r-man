#' Prepare a query request for GFW fishing effort data
#'
#' @description
#' Preliminary function to prepare a 'bait' (API endpoint) to `fish()` for GFW fishing effort data via fishRman's API.
#'
#' @param table Character. The table you want to query. Available tables are 'fishing_effort_v2' (resolution of 0.01 degrees latitude by 0.01 degrees of longitude) and 'fishing_effort_byvessel_v2' (resolution of 0.1 degrees latitude by 0.1 degrees of longitude).
#'
#' @param start_date,end_date Optional. Characters. Date (yyyy-mm-dd) boundaries. Start and end values are included.
#' @param min_lat,max_lat Optional. Numbers. Latitude boundaries. Minimum and maximum values are included.
#' @param min_lon,max_lon Optional. Numbers. Longitude boundaries. Minimum and maximum values are included.
#' @param min_hours,max_hours Optional. Numbers. Minimum and maximum hours (not fishing hours) per grid cell per date. Minimum and maximum values are included.
#' @param min_fishing_hours,max_fishing_hours Optional. Numbers. Minimum and maximum fishing hours per grid cell per date. Minimum and maximum values are included.
#' @param min_mmsi_present,max_mmsi_present Optional. Numbers. Minimum and maximum number of different MMSI numbers present per grid cell per date. Minimum and maximum values are included. Only available for table 'fishing_effort_v2'.
#' @param flag Optional. A character vector. ISO3 Codes of the flag states of the boats streaming the data. Only available for table 'fishing_effort_v2'.
#' @param geartype Optional. A character vector. The main geartype (fishing gear) of the boat streaming the data. Only available for table 'fishing_effort_v2'.
#' @param mmsi Optional. Characters. Pattern (regular expression) to look for in the MMSI number of the boats. Only available for table 'fishing_effort_byvessel_v2'.
#'
#' @returns A character scalar (bait) to pass to `fish()`. The scalar contains the endpoint of the http GET request.
#'
#' @examples
#' \donttest{
#' bait <- bait.gfw.effort(table = "fishing_effort_byvessel_v2", end_date="2012-01-02")
#'
#' print(bait)
#'
#' # "/gfw?table=fishing_effort_v2&end_date=2012-01-02"
#' }
#' @seealso [fish()]
#'
#' @export

bait.gfw.effort <- function(
    table,
    start_date,
    end_date,
    min_lat,
    max_lat,
    min_lon,
    max_lon,
    min_hours,
    max_hours,
    min_fishing_hours,
    max_fishing_hours,
    min_mmsi_present,
    max_mmsi_present,
    flag,
    geartype,
    mmsi
    ) {

  if (missing(table)) stop("You must specify a table")

  # prepare the endpoint of the http request
  endpoint <- paste("/gfw?table=", table, sep = "")

  # date parameter
  if (!missing(start_date) && is.character(start_date)) {
    endpoint = paste(endpoint, "&start_date=", start_date, sep = "")
  }

  if (!missing(end_date) && is.character(end_date)) {
    endpoint = paste(endpoint, "&end_date=", end_date, sep = "")
  }

  # latitude parameter
  if (!missing(min_lat) && is.numeric(min_lat)) {
    endpoint = paste(endpoint, "&min_lat=", min_lat, sep = "")
  }

  if (!missing(max_lat) && is.numeric(max_lat)) {
    endpoint = paste(endpoint, "&max_lat=", max_lat, sep = "")
  }

  # longitude parameter
  if (!missing(min_lon) && is.numeric(min_lon)) {
    endpoint = paste(endpoint, "&min_lon=", min_lon, sep = "")
  }

  if (!missing(max_lon) && is.numeric(max_lon)) {
    endpoint = paste(endpoint, "&max_lon=", max_lon, sep = "")
  }

  # hours parameter
  if (!missing(min_hours) && is.numeric(min_hours)) {
    endpoint = paste(endpoint, "&min_hours=", min_hours, sep = "")
  }

  if (!missing(max_hours) && is.numeric(max_hours)) {
    endpoint = paste(endpoint, "&max_hours=", max_hours, sep = "")
  }

  # fishing_hours parameter
  if (!missing(min_fishing_hours) && is.numeric(min_fishing_hours)) {
    endpoint = paste(endpoint, "&min_fishing_hours=", min_fishing_hours, sep = "")
  }

  if (!missing(max_fishing_hours) && is.numeric(max_fishing_hours)) {
    endpoint = paste(endpoint, "&max_fishing_hours=", max_fishing_hours, sep = "")
  }

  # mmsi_present parameter
  if (!missing(min_mmsi_present) && is.numeric(min_mmsi_present) && table == "fishing_effort_v2") {
    endpoint = paste(endpoint, "&min_mmsi_present=", min_mmsi_present, sep = "")
  }

  if (!missing(max_mmsi_present) && is.numeric(max_mmsi_present) && table == "fishing_effort_v2") {
    endpoint = paste(endpoint, "&max_mmsi_present=", max_mmsi_present, sep = "")
  }

  # flag parameter
  if (all(!missing(flag)) && all(is.character(flag)) && table == "fishing_effort_v2")  {
    flags <- paste(flag, collapse = ",")

    endpoint = paste(endpoint, "&flag=", flags, sep = "")
  }

  # geartype parameter
  if (all(!missing(geartype)) && all(is.character(geartype)) && table == "fishing_effort_v2")  {
    geartypes <- paste(geartype, collapse = ",")

    endpoint = paste(endpoint, "&geartype=", geartypes, sep = "")
  }

  # mmsi parameter
  if (!missing(mmsi) && is.character(mmsi)) {
    endpoint = paste(endpoint, "&mmsi=", mmsi, sep = "")
  }

  # remove all whitespaces possibly introduced
  endpoint <- gsub(" ", "", endpoint)

  # return the bait
  return(endpoint)
}
