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
#' \dontrun{
#' bait <- bait.gfw.effort(table = "fishing_effort_v2", min_lat = 0, flag = c("ITA", "FRA"))
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
                        "Total fishing" = sum(fishing_hours),
                        "Min. fishing" = min(fishing_hours),
                        "1st Qu. fishing" = quantile(fishing_hours, 0.25),
                        "Median fishing" = median(fishing_hours),
                        "Mean fishing" = mean(fishing_hours),
                        "3rd Qu. fishing" = quantile(fishing_hours, 0.75),
                        "Max. fishing" = max(fishing_hours),
                        "Total hours" = sum(hours),
                        "Min. hours" = min(hours),
                        "1st Qu. hours" = quantile(hours, 0.25),
                        "Median hours" = median(hours),
                        "Mean hours" = mean(hours),
                        "3rd Qu. hours" = quantile(hours, 0.75),
                        "Max. hours" = max(hours)
)

  return(summarized)

}
