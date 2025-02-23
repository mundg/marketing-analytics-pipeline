cost_per_conversion <- function(dataframe) {
  sum(dataframe$Cost, na.rm = TRUE) / sum(dataframe$Conversions, na.rm = TRUE)
}