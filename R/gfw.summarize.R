#' Summarize GFW data on fishing effort
#'
#' @importFrom dplyr summarize
#' @importFrom stats median quantile
#'
#' @description
#' Wrapper function for `dplyr::summarise()` that summarizes GFW data into the most important measures of central tendency for fishing_hours and hours, creating a new dataframe. It will have one (or more) rows for each combination of grouping variables; if there are no grouping variables, the output will have a single row summarising all observations in the input.
#'
#' @param df A dataframe object as returned by passing the result of `bait.gfw.effort()` to `fish()`.
#'
#' @returns A dataframe.
#'
#' @examples
#' \donttest{
#' bait <- bait.gfw.effort(table = "fishing_effort_byvessel_v2", end_date="2012-01-02")
#'
#' catch <- fish(bait = bait)
#'
#' summary <- gfw.summarize(catch)
#' }
#'
#' @seealso [bait.gfw.effort()] [fish()] [dplyr::summarise()] [dplyr::group_by()]
#'
#' @export

gfw.summarize <- function(df){

  summarized <- summarize(df,
                        "Total fishing" = sum(.data$fishing_hours),
                        "Min. fishing" = min(.data$fishing_hours),
                        "1st Qu. fishing" = quantile(.data$fishing_hours, 0.25),
                        "Median fishing" = median(.data$fishing_hours),
                        "Mean fishing" = mean(.data$fishing_hours),
                        "3rd Qu. fishing" = quantile(.data$fishing_hours, 0.75),
                        "Max. fishing" = max(.data$fishing_hours),
                        "Total hours" = sum(.data$hours),
                        "Min. hours" = min(.data$hours),
                        "1st Qu. hours" = quantile(.data$hours, 0.25),
                        "Median hours" = median(.data$hours),
                        "Mean hours" = mean(.data$hours),
                        "3rd Qu. hours" = quantile(.data$hours, 0.75),
                        "Max. hours" = max(.data$hours)
)

  return(summarized)

}
