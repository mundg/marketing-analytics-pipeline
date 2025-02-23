library(jsonlite)

load_last_processed_dates <- function(state_file_path) {
  if (file.exists(state_file_path)) {
    return(fromJSON(state_file_path))
  }
  return(list())
}

save_last_processed_dates <- function(state_file_path, dates_list) {
  return(write_json(dates_list, path = state_file_path, pretty = TRUE))
}
