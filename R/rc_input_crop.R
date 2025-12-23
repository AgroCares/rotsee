#' Do check and expand crop input table for RothC for the Netherlands
#'
#' Helper function to check the content and format of the crop input table.
#'
#' @param dt (data.table) Table with crop rotation and related crop properties for Carbon input. See details for information.
#'
#' @details
#' The crop table used as input for carbon modelling requires at minimum data on effective organic matter inputs and related year.
#' To run this function, the dt should contain the following columns:
#' * B_LU_START (date/character), start of crop growth (formatted YYYY-MM-DD)
#' * B_LU_END (date/character), end of crop growth (formatted YYYY-MM-DD)
#' * B_LU_HC (the humification coefficient of crop organic matter (-). When not supplied, default RothC value will be used)
#' * B_C_OF_INPUT (the organic carbon input on field level (kg C/ha). In case not known, can be calculated using function \link{rc_calculate_bcof})
#'
#' @export
rc_input_crop <- function(dt){
  
  # add visual bindings
  B_C_OF_INPUT  = B_LU_END  = B_LU_HC = B_LU_START = cin_dpm = NULL
    cin_rpm = fr_dpm_rpm = NULL
  
  # check crop table
  checkmate::assert_data_table(dt,null.ok = FALSE)
  req <- c("B_LU_START", "B_LU_END", "B_LU_HC","B_C_OF_INPUT")
  checkmate::assert_names(colnames(dt), must.include = req)
  checkmate::assert_date(as.Date(dt$B_LU_START), any.missing = FALSE)
  checkmate::assert_date(as.Date(dt$B_LU_END), any.missing = FALSE)
  checkmate::assert_numeric(dt$B_C_OF_INPUT, any.missing = FALSE, lower = rc_minval("B_C_OF_INPUT"), upper = rc_maxval("B_C_OF_INPUT"))
  
  # create a copy of the crop table
  dt.crop <- copy(dt)

    # Get year and month for the end of crop rotation
  dt.crop[,year := year(B_LU_END)]
  dt.crop[,month := month(B_LU_END)]

  # ensure that year always start with 1 to X, and sort
  setorder(dt.crop,year, month)
  
  # add dpm-rmp ratio
  dt.crop[,fr_dpm_rpm := fifelse(B_LU_HC < 0.92, -2.174 * B_LU_HC + 2.02, 0)]
  
  # replace dpm-rpm ratio with defaults when hc is missing
  dt.crop[is.na(fr_dpm_rpm), fr_dpm_rpm := 1.44]
  
  # estimate total Carbon input per crop and year (kg C / ha)

    # Estimate averaged C input for DPM and RDM pool (kg C / ha)
    dt.crop[,cin_dpm := B_C_OF_INPUT * fr_dpm_rpm / (1 + fr_dpm_rpm)]
    dt.crop[,cin_rpm := B_C_OF_INPUT * 1 / (1 + fr_dpm_rpm)]

  # select only relevant columns with C input (kg C/ ha)
  dt.crop <- dt.crop[,list(year = year, month = month, B_LU_END, B_LU_START, cin_dpm, cin_rpm)]
  
  # return
  return(dt.crop)
}
