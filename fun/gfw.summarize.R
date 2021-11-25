# specific instance of the dplyr::summarize() function, with important measures
# of central tendency for fishing_hours and hours

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