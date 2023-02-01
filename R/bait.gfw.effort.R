#' Prepare a query request for GFW fishing effort data
#'
#' @importFrom jsonlite toJSON
#'
#' @description
#' Preliminary function to prepare a 'bait' with all the necessary components to `fish()` for GFW fishing effort data via fishRman's API.
#'
#' @param table Character. The table you want to query. Available tables are 'fishing_effort_v2' (resolution of 0.01 degrees latitude by 0.01 degrees of longitude) and 'fishing_effort_byvessel_v2' (resolution of 0.1 degrees latitude by 0.1 degrees of longitude).
#'
#' @param start_date,end_date Optional. Date (yyyy-mm-dd) boundaries. Start and end values are included.
#' @param min_lat,max_lat Optional. Numbers. Latitude boundaries. Minimum and maximum values are included.
#' @param min_lon,max_lon Optional. Numbers. Longitude boundaries. Minimum and maximum values are included.
#' @param min_hours,max_hours Optional. Numbers. Minimum and maximum hours (not fishing hours) per grid cell per date. Minimum and maximum values are included.
#' @param min_fishing_hours,max_fishing_hours Optional. Numbers. Minimum and maximum fishing hours per grid cell per date. Minimum and maximum values are included.
#' @param min_mmsi_present,max_mmsi_present Optional. Numbers. Minimum and maximum number of different MMSI numbers present per grid cell per date. Minimum and maximum values are included. Only available for table 'fishing_effort_v2'.
#' @param flag Optional. A character vector. ISO3 Codes of the flag states of the boats streaming the data. Only available for table 'fishing_effort_v2'.
#' @param geartype Optional. A character vector. The main geartype (fishing gear) of the boat streaming the data. Only available for table 'fishing_effort_v2'.
#' @param mmsi Optional. A character vector. Pattern (regular expression) to look for in the MMSI number of the boats. Only available for table 'fishing_effort_byvessel_v2'.
#'
#' @returns A list (bait) to pass to `fish()`. The list contains the endpoint of the http request, the body of the request, and the SQL that will be run.
#'
#' @examples
#' bait <- bait.gfw.effort(table = "fishing_effort_v2", min_lat = 0, flag = c("ITA", "FRA"))
#'
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

  endpoint <- "/gfw"

  # prepare the body of the http request by listing all args passed to the function

  body <- as.list(environment())

  body <- toJSON(body[!sapply(body, is.symbol)], auto_unbox = TRUE)

  # begin to prepare the SQL the user will be able to check before sending the http request

  SQL <- "SELECT * FROM global-fishing-watch.gfw_public_data."

  # table parameter

  SQL <- paste(SQL, table, sep = "")

  # date parameter

  if (!missing(start_date) && !is.null(start_date) && !is.na(start_date) && is.character(start_date)) {

    SQLAddition <- sprintf("AND date >= '%s'", start_date)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  if (!missing(end_date) && !is.null(end_date) && !is.na(end_date) && is.character(end_date)) {

    SQLAddition <- sprintf("AND date <= '%s'", end_date)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  # function to make next if statements easier

  is.valid.numeric <- function(x) {

    if (!missing(x) && !is.null(x) && !is.na(x) && is.numeric(x)) { return(T) } else { return(F) }

  }

  # latitude parameter

  if (is.valid.numeric(min_lat)) {

    SQLAddition <- sprintf("AND cell_ll_lat >= %s", min_lat)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  if (is.valid.numeric(max_lat)) {

    SQLAddition <- sprintf("AND cell_ll_lat <= %s", max_lat)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  # longitude parameter

  if (is.valid.numeric(min_lon)) {

    SQLAddition <- sprintf("AND cell_ll_lon >= %s", min_lon)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  if (is.valid.numeric(max_lon)) {

    SQLAddition <- sprintf("AND cell_ll_lon <= %s", max_lon)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  # hours parameter

  if (is.valid.numeric(min_hours)) {

    SQLAddition <- sprintf("AND hours >= %s", min_hours)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  if (is.valid.numeric(max_hours)) {

    SQLAddition <- sprintf("AND hours <= %s", max_hours)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  # fishing_hours parameter

  if (is.valid.numeric(min_fishing_hours)) {

    SQLAddition <- sprintf("AND fishing_hours >= %s", min_fishing_hours)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  if (is.valid.numeric(max_fishing_hours)) {

    SQLAddition <- sprintf("AND fishing_hours <= %s", max_fishing_hours)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  # mmsi_present parameter

  if (is.valid.numeric(min_mmsi_present) && table == "fishing_effort_v2") {

    SQLAddition <- sprintf("AND mmsi_present >= %s", min_mmsi_present)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  if (is.valid.numeric(max_mmsi_present) && table == "fishing_effort_v2") {

    SQLAddition <- sprintf("AND mmsi_present <= %s", max_mmsi_present)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  # flag parameter

  if (all(!missing(flag)) && all(!is.null(flag)) && all(!is.na(flag)) && all(is.character(flag)) && table == "fishing_effort_v2")  {

    flags <- paste(flag, collapse = "', '")

    SQLAddition <- sprintf("AND flag IN ('%s')", flags)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  # geartype parameter

  if (all(!missing(geartype)) && all(!is.null(geartype)) && all(!is.na(geartype)) && all(is.character(geartype)) && table == "fishing_effort_v2")  {

    geartypes <- paste(geartype, collapse = "', '")

    SQLAddition <- sprintf("AND geartype IN ('%s')", geartypes)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  # mmsi parameter

  if (!missing(mmsi) && !is.null(mmsi) && !is.na(mmsi) && is.character(mmsi)) {

    SQLAddition <- sprintf("AND mmsi LIKE '%s'", mmsi)

    SQL <- paste(SQL, SQLAddition, sep = " ")

  }

  # substitute the first AND, if any, with WHERE

  SQL <- sub("AND", "WHERE", SQL)

  # package the whole bait

  bait <- list(endpoint, body, SQL)

}
