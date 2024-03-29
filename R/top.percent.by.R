#' Subset the top percent of a dataframe by a specific column
#'
#' @importFrom dplyr pull arrange
#'
#' @description
#' Function that sorts a dataframe in descending order for a specific column, calculates the sum of all rows for that column, applies the chosen percentage to said sum, and subsets the minimum number of consecutive rows needed to reach this value.
#'
#' @param df A dataframe object as downloaded from GFW's Google Big Data Query.
#' @param percentage Number. The 'x' in 'the top x percent of the dataframe'.
#' @param by Character. The name of the column for which the percentage will be calculated.
#'
#' @returns A dataframe.
#'
#' @examples
#'
#' dated <- c("2020-01-01", "2020-01-02")
#' lat <- c(40, 41)
#' lon <- c(12,13)
#' mmsi <- c("34534555", "25634555")
#' hours <- c(0, 5)
#' fishing_hours <- c(1,9)
#'
#' df <- data.frame(dated, lat, lon, mmsi, hours, fishing_hours)
#'
#' who.fishs.the.most <- top.percent.by(df, 90, "fishing_hours")
#'
#' print(who.fishs.the.most)
#'
#' @export

top.percent.by <- function(df, percentage, by) {

  # this is a function to count the length of a vector necessary until it reaches an amount
  # used for cumulative distribution
  ## source: https://stackoverflow.com/questions/8540143/add-consecutive-elements-of-a-vector-until-a-value

  length.until <- function(x, max = 10) {
    s <- 0
    len <- length(x) # modified this to avoid function looping forever in case of bad
    # rounding or similar minor issues. This way, length is, at most, equal to the entire dataset
    start <- 1
    j <- 1
    for (i in seq_along(x)) {
      s <- s + x[i]
      while (s >= max) {
        if (i - j + 1 < len) {
          len <- i - j + 1
          start <- j
        }
        s <- s - x[j]
        j <- j + 1
      }
    }

    # commented the list below because we only need len
    # list(start = start, length = len)

    len
  }

  # sort df according to 'by' parameter, decreasing
  df <- arrange(df, desc(!!sym(by))) # formerly df <- df[order(-df[by]), ] #used dplyr::arrange instead of order

  # calculate sum of the entire column named as the 'by' parameter and find its 'percentage'
  summed <- sum(df[by]) * (percentage / 100)

  # vectorize the column, to be able to use the length.until function
  by <- pull(df, by)

  # how many rows are needed to reach the 'percentage' of the total for the 'by' parameter?
  if (summed != 0) {
    l <- length.until(by, summed)
  } else {
    l <- nrow(df)
  }

  # subsetting df accordingly
  df <- df[c(1:l), ]

  return(df)

}
