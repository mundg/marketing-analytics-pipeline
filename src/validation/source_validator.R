
validate_missing_cols <- function(dataframe, config_schema, source){
  # ========================================================
  # Validate columns in the Dataframe to their expected data
  # ========================================================
  missing_cols <- setdiff(config_schema$schemas[[source_name]]$columns,
                          colnames(data))
  if (length(missing_cols) > 0) {
    message("Missing columns: ", paste(missing_cols, collapse = ", "))
  }
}


validate_dtypes_cols <- function(dataframe, config_schema, source){
  # ========================================================
  # Validate column's expected data type 
  # ========================================================
  expected_types <- config_schema$schemas[[source]]$column_types
  for (col in names(expected_types)){
    expected_dtype <- expected_types[[col]]
    dataframe_col_dtype <- class(dataframe[[col]])
    if (dataframe_col_dtype != expected_dtype){
      message("Source: ",source,", Warning: Column ", col, " expected ", expected_dtype, 
              " but found ", dataframe_col_dtype)
    }
  }
}