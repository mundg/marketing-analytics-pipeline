library(lubridate)
source('./src/aggregation/metric_functions.R')

aggregate_by_channel <- function(dataframe, metric_func = cost_per_conversion){
  aggregated_data <- dataframe %>%
    group_by(Channel) %>%
    summarise(cost_per_conversions = metric_func(.data), .groups = "drop")
  return(aggregated_data)
}


aggregate_by_week <- function(dataframe, metric_func = cost_per_conversion) {
  aggregated_data <- dataframe %>%
    mutate(Week = floor_date(dataframe$Date, "week")) %>%  
    group_by(Week) %>%
    summarise(cost_per_conversions = metric_func(.data), .groups = "drop") %>% 
    arrange(Week)
  return(aggregated_data)
}