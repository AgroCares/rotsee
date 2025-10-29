# This script updates the internal parameter table (parameters.rda) from the CSV file
  # read in location of csv file
  csv_path <- "data-raw/rc_params.csv"
  
  # check if CSV file exists
  if(!file.exists(csv_path)){
    stop("CSV file not found at: ", csv_path, call. = FALSE)
  }
  
  # read in table with relevant parameters
  rc_params <- data.table::fread(csv_path)
  
  # validate required columns exist
  required_cols <- c("code", "value_min", "value_max")
  missing_cols <- setdiff(required_cols, names(rc_params))
  if (length(missing_cols) > 0) {
    stop("CSV is missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
    }
    
    # validate data types
    checkmate::assert_character(rc_params$code, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(rc_params$value_min, any.missing = FALSE)
  checkmate::assert_numeric(rc_params$value_max, any.missing = FALSE)
  
  # save parameter table as rda file
  usethis::use_data(rc_params, overwrite = TRUE, compress = "xz")
  
  message("Parameter table updated in data/. Rebuild the package for changes to take effect.")
  
