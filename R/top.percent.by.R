#' Subset the top percent of a dataframe by a specific column
#'
#' @importFrom dplyr pull arrange
#'
#' @description
#' Function that sorts a dataframe in descending order for a specific column, calculates the sum of all rows for that column, applies the chosen percentage to said sum, and subsets the minimum number of consecutive rows needed to reach this value.
#'
#' @param df A dataframe object as returned by passing the result of `bait.gfw.effort()` to `fish()`.
#' @param percentage Number. The 'x' in 'the top x percent of the dataframe'.
#' @param by Character. The name of the column for which the percentage will be calculated.
#'
#' @returns A dataframe.
#'
#' @examples
#' bait <- bait.gfw.effort(table = "fishing_effort_v2", min_lat = 0, flag = c("ITA", "FRA"))
#'
#' catch <- fish(bait = bait)
#'
#' who.fishs.the.most <- top.percent.by(catch, 90, "fishing_hours")
#'
#' @seealso [bait.gfw.effort()] [fish()]
#'
#' @export

top.percent.by <- function(df, percentage, by) {

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
