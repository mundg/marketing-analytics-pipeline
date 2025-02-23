

clean_drop_duplicates <- function(dataframe){
  # ========================================================
  # Returns Unique Rows
  # ========================================================
  return (unique(dataframe))
}


transform_data_pivot <- function(dataframe, config_schema, source){
  # ========================================================
  # Returns pivoted data of sources that requires transformation
  # ========================================================
  is_pivot_enabled <- purrr::pluck(config_schema, 'schemas', source,
                                 'transform', 'pivot','enabled',  .default = FALSE)
  if (is_pivot_enabled) {
    dataframe <- dataframe %>%     
      tidyr::pivot_wider(
        names_from = config_schema$schemas[[source]]$transform$pivot$column,
        values_from = config_schema$schemas[[source]]$transform$pivot$values,
        values_fn = list(value = sum)
      )
  }
  return (dataframe)
}



clean_data_columns <- function(dataframe, config_schema, source) {
  # ========================================================
  # Returns clean data, required data types & columns names
  # ========================================================
  is_cleaning_enabled <- purrr::pluck(config_schema, 'schemas', source_name,
                                 'transform', 'clean', 'enabled', .default = FALSE)
  if (is_cleaning_enabled) {
    cleaning_rules <- config_schema$schemas[[source_name]]$transform$clean
    
    if ('clean_columns' %in% names(cleaning_rules)) {
      for (col in names(cleaning_rules$clean_columns)) {
        
        col_cleaning_rules <- cleaning_rules$clean_columns[[col]]
        
        if ('remove_character' %in% names(col_cleaning_rules)){
          dataframe[[col]] <- stringr::str_replace(dataframe[[col]], stringr::fixed(col_cleaning_rules$remove_character), '')
        }
        if ('type' %in% names(col_cleaning_rules)){
          
          if (col_cleaning_rules$type == 'integer'){
            dataframe[[col]] <- as.integer(dataframe[[col]])
          } else if (col_cleaning_rules$type == 'numeric'){
            dataframe[[col]] <- as.numeric(dataframe[[col]])
          }
          
        }
      }
    }
    if ('rename_columns' %in% names(cleaning_rules)){
      dataframe <- dataframe %>% dplyr::rename_with(~cleaning_rules$rename_columns$new_name,
                                                    all_of(cleaning_rules$rename_columns$old_name))
    }
  }
  return (dataframe)
}