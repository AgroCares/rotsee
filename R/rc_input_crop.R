#' Do check and expand crop input table for RothC for the Netherlands
#'
#' Helper function to check the content and format of the crop input table.
#'
#' @param dt (data.table) Table with crop rotation and related crop properties for Carbon input. See details for information.
#' @param cf_yield (numeric) A yield correction factor (fraction) if yield is higher than regional average
#'
#' @details
#' The crop table used as input for carbon modelling requires at minimum data on effective organic matter inputs and related year.
#' To run this function, the dt should contain the following columns:
#' * year
#' * month
#' * B_LU (a crop id)
#' * B_LU_NAME (a crop name, optional):
#' * B_LU_HC (the humification coefficient of crop organic matter (-). When not supplied, default RothC value will be used)
#' * B_C_OF_INPUT (the organic carbon input on field level (kg C/ha). In case not known, can be calculated using function \link{rc_calculate_B_C_OF})
#'
#' @export
rc_input_crop <- function(dt, cf_yield = 1){
  # add visual bindings
  M_GREEN_TIMING =  M_IRRIGATION = M_CROPRESIDUE = cin_dpm = B_C_OF_INPUT = cin_rpm = NULL
  CF_YIELD = YEAR = crft = B_LU  = fr_dpm_rpm = B_LU_HC = B_LU_HI_RES = B_LU_START = B_LU_END = NULL
  cin_aboveground = B_LU_YIELD =B_LU_DM = B_LU_HI = cin_roots = B_LU_RS_FR = cin_residue = NULL
  
  # check crop table
  checkmate::assert_data_table(dt,null.ok = TRUE)
  req <- c("B_LU_START", "B_LU_END", "B_LU","B_LU_HC","B_C_OF_INPUT")
  checkmate::assert_true(all(req %in% names(dt)))
  checkmate::assert_numeric(cf_yield,lower = 0.1, upper = 2.0, any.missing = FALSE,len = 1)
  
  # create a copy of the crop table
  dt.crop <- copy(dt)
  
  # Add month column when not supplied
  if(!"month" %in% colnames(dt.crop)) {
    dt.crop[, month := NA_integer_]}
    
  
  # update crop basic properties
  if(!'M_GREEN_TIMING' %in% colnames(dt.crop)){dt.crop[,M_GREEN_TIMING := 'never']}
  if(!'M_CROPRESIDUE' %in% colnames(dt.crop)){dt.crop[,M_CROPRESIDUE := FALSE]}
  if(!'M_IRRIGATION' %in% colnames(dt.crop)){dt.crop[,M_IRRIGATION := FALSE]}
  if(!'CF_YIELD' %in% colnames(dt.crop)){dt.crop[,CF_YIELD := cf_yield[1]]}
  
  # Get year and month for the end of crop rotation
  dt.crop[,year := year(B_LU_END)]
  dt.crop[,month := month(B_LU_END)]

  # ensure that year always start with 1 to X, and sort
  setorder(dt.crop,year)
  
  # add dpm-rmp ratio
  dt.crop[,fr_dpm_rpm := fifelse(B_LU_HC < 0.92, -2.174 * B_LU_HC + 2.02, 0)]
  
  # replace dpm-rpm ratio with defaults when hc is missing
  dt.crop[is.na(fr_dpm_rpm), fr_dpm_rpm := 1.44]
  
  # estimate total Carbon input per crop and year (kg C / ha)

    # Estimate averaged C input for DPM and RDM pool (kg C / ha)
    dt.crop[,cin_dpm := B_C_OF_INPUT * fr_dpm_rpm / (1 + fr_dpm_rpm)]
    dt.crop[,cin_rpm := B_C_OF_INPUT * 1 / (1 + fr_dpm_rpm)]


  # select only relevant columns with C input (kg C/ ha)
  dt.crop <- dt.crop[,list(year = year, month = month, B_LU_END, B_LU_START, B_LU,cin_dpm, cin_rpm,
                           M_GREEN_TIMING,M_IRRIGATION,M_CROPRESIDUE,
                           cf_yield = CF_YIELD)]
  
  # return
  return(dt.crop)
}
