library(ggplot2)
library(hrbrthemes)
library(scales)

## Cost Over Time
line_cost_over_time <- function(dataframe, path){
  plot <- dataframe %>% mutate(Week = floor_date(dataframe$Date, "week")) %>%  
    group_by(Week) %>%
    summarise(Cost = sum(Cost, na.rm = TRUE), .groups = "drop") %>% 
    arrange(Week) %>%
  ggplot(aes(x = Week, y = Cost)) + 
    geom_line( color="grey") +
    geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
    theme_ipsum(
      base_family = "sans",  
      plot_title_family = "sans",
      subtitle_family = "sans"
    ) + 
    scale_y_continuous(labels = scales::dollar_format()) +
    ggtitle("Cost Over Time")
  ggsave(plot=plot, filename=path, bg = 'white', width = 8, height = 4, dpi = 300)
}

## Conversions by Channel
bar_conversions_by_channel <- function(dataframe, path){
  plot <- dataframe %>% 
    group_by(Channel) %>% 
    summarise(Conversions = sum(Conversions, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = Channel, y = Conversions)) + 
    geom_bar(stat = 'identity', fill = '#69b3a2') +
    theme_ipsum(
      base_family = "sans",  
      plot_title_family = "sans",
      subtitle_family = "sans"
    ) +
    guides(x = guide_axis(angle=45)) +
    scale_y_continuous(labels = comma) +
    ggtitle("Conversions by Channel")
  ggsave(plot=plot, filename=path, bg = 'white', width = 8, height = 4, dpi = 300)
}


## Cost vs Conversions

scatter_cost_vs_conversions <- function(dataframe, path){
  plot <- dataframe %>%
    ggplot(aes(x = Cost, y = Conversions)) + 
    geom_point(color = 'tomato3') +
    theme_ipsum(
      base_family = "sans",  
      plot_title_family = "sans",
      subtitle_family = "sans"
    ) +
    scale_y_continuous(labels = comma) +
    scale_x_continuous(labels = scales::dollar_format()) +
    ggtitle("Cost vs Conversions")
  ggsave(plot=plot, filename=path, bg = 'white', width = 8, height = 6, dpi = 300)
}


## Average Daily Conversions by Day of the Week 

average_daily_conversions <- function(dataframe, path){
  plot <- dataframe %>% 
    mutate(day_of_week = weekdays(Date)) %>%
    mutate(day_of_week = factor(day_of_week, 
                                levels = c("Monday", "Tuesday", "Wednesday", "Thursday", 
                                           "Friday", "Saturday", "Sunday"))) %>%
    group_by(day_of_week) %>%
    summarise(average_conversions = mean(Conversions, na.rm = TRUE), .groups = "drop") %>%
    arrange(day_of_week) %>%
    ggplot(aes(x = day_of_week, y = average_conversions)) + 
    geom_bar(stat = 'identity', fill = 'tomato3') +
    theme_ipsum(
      base_family = "sans",  
      plot_title_family = "sans",
      subtitle_family = "sans"
    ) +
    guides(x = guide_axis(angle=45)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
    ggtitle("Average Conversions by Day of the Week")
  ggsave(plot=plot, filename=path, bg = 'white', width = 7, height = 6, dpi = 300)
    
}


## Conversions by Audience Type (Bar Chart)

bar_conversions_by_audience <- function(dataframe, path){
  plot <- dataframe %>% 
    group_by(Target) %>% 
    summarise(Conversions = sum(Conversions, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = Target, y = Conversions)) + 
    geom_bar(stat = 'identity', fill = 'tomato3') +
    theme_ipsum(
      base_family = "sans",  
      plot_title_family = "sans",
      subtitle_family = "sans"
    ) +
    guides(x = guide_axis(angle=45)) +
    scale_y_continuous(labels = comma) +
    ggtitle("Conversions by Audience Type")
  ggsave(plot=plot, filename=path, bg = 'white', width = 5, height = 5, dpi = 300)
}




