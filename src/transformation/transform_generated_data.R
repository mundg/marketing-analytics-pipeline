
convert_generated_data_dtypes <- function(dataframe, config_schema) {
  expected_types <- config_schema$column_types
  for (col in names(dataframe)){
    expected_dtype <- expected_types[[col]]
    if (expected_dtype == 'numeric'){
      dataframe[[col]] <- as.numeric(dataframe[[col]])
    } else if (expected_dtype == 'numeric') {
      dataframe[[col]] <- as.integer(dataframe[[col]])
    } else if (expected_dtype == 'character') {
      dataframe[[col]] <- as.character(dataframe[[col]])
    } else if (expected_dtype == 'Date') {
      dataframe[[col]] <- as.Date(dataframe[[col]])
    } else {
      dataframe[[col]]
    }
  }
  return(dataframe)
}