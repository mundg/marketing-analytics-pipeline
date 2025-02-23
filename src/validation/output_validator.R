validate_processed_cols <- function(dataframe, config_schema){
  # ========================================================
  # Validate columns of the output Dataframe
  # ========================================================
  missing_cols <- setdiff(names(config_schema$column_types),
                          colnames(data))
  if (length(missing_cols) > 0) {
    message("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
}


validate_processed_dtypes <- function(dataframe, config_schema){
  # ========================================================
  # Validate columns dtypes of the output Dataframe
  # ========================================================
  for (col in names(config_schema$column_types)){
    expected_dtype <- config_schema$column_types[[col]]
    dataframe_col_dtype <- class(dataframe[[col]])
    if (dataframe_col_dtype != expected_dtype){
      message("Source: ",source,", Warning: Column ", col, " expected ", expected_dtype, 
              " but found ", dataframe_col_dtype)
    }
  }
}