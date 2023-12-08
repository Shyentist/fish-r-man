#' Check the type of dataframe in relation to fishRman
#'
#' @description
#' Function to check the type of dataframe in relation to fishRman. Most of the times the user will not call this function directly, but it can be useful for debug.
#'
#' @param df The dataframe to check.
#'
#' @returns A string explaining how the input dataframe is treated by fishRman.
#'
#' @examples
#'
#' dated <- c("2020-01-01", "2020-01-02")
#' lat <- c(40, 41)
#' lon <- c(12,13)
#' mmsi <- c("34534555", "25634555")
#' hours <- c(0, 5)
#' fishing_hours <- c(1,2)
#'
#' df <- data.frame(dated, lat, lon, mmsi, hours, fishing_hours)
#'
#' what.type <- df.type(df)
#'
#' print(what.type)
#'
#' # "GFW Fishing Effort By Vessel"
#' @seealso [bait.gfw.effort()] [fish()]
#'
#' @export

df.type <- function(df) {
  # the col names for different types of df are here so I can check against them

  gfw.effort.colnames <- c(
    "dated",
    "lat",
    "lon",
    "flag",
    "geartype",
    "hours",
    "fishing_hours",
    "mmsi_present"
  )

  gfw.effort.sf.colnames <- c(
    "dated",
    "lat",
    "lon",
    "flag",
    "geartype",
    "hours",
    "fishing_hours",
    "mmsi_present",
    "geometry"
  )

  gfw.effort.byvessel.colnames <- c(
    "dated",
    "lat",
    "lon",
    "mmsi",
    "hours",
    "fishing_hours"
  )

  gfw.effort.byvessel.sf.colnames <- c(
    "dated",
    "lat",
    "lon",
    "mmsi",
    "hours",
    "fishing_hours",
    "geometry"
  )

  sorted.colnames <- sort(colnames(df))

  if (identical(sorted.colnames, sort(gfw.effort.colnames))) {
    return("GFW Fishing Effort")
  } else if (identical(sorted.colnames, sort(gfw.effort.byvessel.colnames))) {
    return("GFW Fishing Effort By Vessel")
  } else if (identical(sorted.colnames, sort(gfw.effort.sf.colnames))) {
    return("GFW Fishing Effort (as Simple Feature)")
  } else if (identical(sorted.colnames, sort(gfw.effort.byvessel.sf.colnames))){
    return("GFW Fishing Effort By Vessel (as Simple Feature)")
  } else {
    return("Invalid")
  }

}
