library(yaml)
library(dplyr)
library(tidyr)
library(here)
library(jsonlite)

source('./src/data_loaders/csv_loader.R')
source('./src/validation/source_validator.R')
source('./src/validation/output_validator.R')
source('./src/transformation/transform_source_data.R')
source('./src/transformation/transform_generated_data.R')
source('./src/aggregation/aggregate_data.R')
source('./src/visualization/chart_generator.R')
source('./src/utils/helper.R')

main <- function(x){

source_config <- yaml.load_file('./config/source_config.yml')
source_schemas <- yaml.load_file('./config/source_schemas.yml')
output_config <- yaml.load_file('./config/output_config.yml')
logging_config <- yaml.load_file('./config/logging_config.yml')

# Last processed dates
last_processed_dates <- load_last_processed_dates(state_file_path = logging_config$logging$source_log_path)

# Loop in Data Sources
all_sources <- list()

for (source_name in names(source_config$sources)) {
  data <- load_from_csv(source_config$sources[[source_name]]$path)
  
  # Convert to dates
  date_column_config <- source_schemas$schemas[[source_name]]$date_column
  data[[date_column_config]] <- as.Date(data[[date_column_config]], format = "%d-%b-%y")
  # 
  if(source_name %in% names(last_processed_dates)){
    data <- data[data[[date_column_config]] > as.Date(last_processed_dates[[source_name]]), ]
  }
  
  if(nrow(data) == 0){
    message('No new date for channel source: ', source_name)
    next
  } 
  
  # Validate Source Data
  validate_missing_cols(data, source_schemas, source_name) 
  validate_dtypes_cols(data, source_schemas, source_name) 
  
  # Clean and Transform Data
  data <- clean_drop_duplicates(data) 
  data <-  transform_data_pivot(data, source_schemas, source_name)
  data <- clean_data_columns(data, source_schemas, source_name)
  
  # Save last recent date processed 
  max_date <- max(data$Date, na.rm = TRUE)
  if (!is.na(max_date)){
    last_processed_dates[[source_name]] <- max_date
  }

  # Add the data into the list
  all_sources[[length(all_sources) + 1]] <- data
}

save_last_processed_dates(state_file_path = logging_config$logging$source_log_path,
                          dates_list = last_processed_dates)

final_df <- bind_rows(all_sources)

# Final Validate overall output 
if (nrow(final_df) > 0){
  validate_processed_cols(final_df, output_config$validation)
  validate_processed_dtypes(final_df, output_config$validation)
}


file_path <- output_config$output_paths$final_dataset
if (file.exists(file_path)) {
  existing_df <- load_from_csv(file_path)
  existing_df <- convert_generated_data_dtypes(existing_df, output_config$validation)
  final_df <- bind_rows(final_df, existing_df)
}

# Write CSVs
write.csv(final_df, file_path, row.names = FALSE)
write.csv(aggregate_by_channel(final_df), output_config$output_paths$reports$channel_performance, row.names = FALSE)
write.csv(aggregate_by_week(final_df), output_config$output_paths$reports$weekly_performance, row.names = FALSE)

# Generate and save charts
line_cost_over_time(final_df, output_config$output_paths$charts$cost_over_time)
bar_conversions_by_channel(final_df, output_config$output_paths$charts$conversions_per_channel)
scatter_cost_vs_conversions(final_df, output_config$output_paths$charts$cost_vs_conversions)
average_daily_conversions(final_df, output_config$output_paths$charts$average_daily_conversions)
bar_conversions_by_audience(final_df, output_config$output_paths$charts$conversions_by_audience)

}


# Run Script
main()
