# This script updates the internal parameter table (parameters.rda) from the CSV file
  # read in location of csv file
  csv_path <- "data-raw/rothc_params.csv"
  
  # check if CSV file exists
  if(!file.exists(csv_path)){
    stop("CSV file not found at: ", csv_path, call. = FALSE)
  }
  
  # read in table with relevant parameters
  parameters <- data.table::fread(csv_path)
  
  # validate required columns exist
  required_cols <- c("code", "value_min", "value_max")
  missing_cols <- setdiff(required_cols, names(parameters))
  if (length(missing_cols) > 0) {
    stop("CSV is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    
    # validate data types
    checkmate::assert_character(parameters$code, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(parameters$value_min, any.missing = FALSE)
  checkmate::assert_numeric(parameters$value_max, any.missing = FALSE)
  
  # save parameter table as rda file
  usethis::use_data(parameters, overwrite = TRUE, compress = "xz")
  
  message("Parameter table updated in data/. Rebuild the package for changes to take effect.")
  
