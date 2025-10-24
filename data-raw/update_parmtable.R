# This script updates the internal parameter table (parameter.rda) from the CSV file
  # read in location of csv file
  csv_path <- "data-raw/rothc_params.csv"
  
  # check if CSV file exists
  if(!file.exists(csv_path)){
    stop("CSV file not found at: ", csv_path, call. = FALSE)
  }
  
  # read in table with relevant parameters
  parameters <- data.table::fread(csv_path)
  
  # save parameter table as rda file
  usethis::use_data(parameters, overwrite = TRUE, compress = "xz")
  
  message("Parameter table updated in data/. Rebuild the package for changes to take effect.")
  
