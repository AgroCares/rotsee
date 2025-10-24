#' Do check and expand amendment input table for RothC
#'
#' Helper function to check the content and format of the amendment input table.
#'
#' @param dt (data.table) Table with amendments and amendment properties for Carbon input.
#'
#' @details
#' The amendments table used as input for carbon modelling requires at minimum data on effective organic matter inputs and related year.
#' This helper function assists the checking and controlling of amendments properties involved.
#'
#' rothc_amendment: amendment table
#' Includes the columns:
#' * B_C_OF_INPUT (numeric), the organic carbon input from soil amendment product on a field level (kg C/ha)
#' * P_DOSE (numeric), applied dose of soil amendment product (kg/ha), required if B_C_OF_INPUT is not supplied
#' * P_C_OF (numeric), organic carbon content of the soil amendment product (g C/kg), required if B_C_OF_INPUT is not supplied
#' * P_HC (numeric), the humification coefficient of the soil amendment product (fraction)
#' * P_DATE_FERTILIZATION (date), date of fertilizer application (formatted YYYY-MM-DD)
#'
#' @export
rc_input_amendment <- function(dt = NULL){
  

  # add visual bindings
  P_C_OF = B_C_OF_INPUT = P_DATE_FERTILIZATION = P_DOSE = P_HC =  NULL
  cin_dpm = cin_hum = cin_rpm = cin_tot = fr_dpm_rpm = NULL
  
  # Check amendment table
  checkmate::assert_data_table(dt, null.ok = FALSE, min.rows = 1)
  
  req <- c("P_HC","P_DATE_FERTILIZATION")
  checkmate::assert_names(names(dt), must.include = req)
  checkmate::assert_date(as.Date(dt$P_DATE_FERTILIZATION), any.missing = FALSE)
  checkmate::assert_numeric(dt$P_HC, lower = rc_minval("P_HC"), upper = rc_maxval("P_HC"), any.missing = FALSE)
  if ("B_C_OF_INPUT" %in% names(dt)) { 
    # Validate B_C_OF_INPUT
    checkmate::assert_numeric(dt$B_C_OF_INPUT, lower = rc_minval("B_C_OF_INPUT"), upper = rc_maxval("B_C_OF_INPUT"), any.missing = TRUE)
    # Check P_DOSE and P_C_OF when B_C_OF_INPUT is missing
      if (anyNA(dt$B_C_OF_INPUT)) {
         checkmate::assert(
          all(c("P_DOSE","P_C_OF") %in% names(dt)),
          msg = "For rows with NA B_C_OF_INPUT, both P_DOSE and P_C_OF must be provided"
          )
        checkmate::assert(
          all(!is.na(dt$P_DOSE[is.na(dt$B_C_OF_INPUT)]) &
              !is.na(dt$P_C_OF[is.na(dt$B_C_OF_INPUT)])),
          msg = "For rows with NA B_C_OF_INPUT, both P_DOSE and P_C_OF must be provided"
          )
      }
    # Check P_DOSE and P_C_OF
    if ("P_DOSE" %in% names(dt)) checkmate::assert_numeric(dt$P_DOSE, lower = rc_minval("P_DOSE"), upper = rc_maxval("P_DOSE"), any.missing = TRUE)
    if ("P_C_OF" %in% names(dt)) checkmate::assert_numeric(dt$P_C_OF, lower = rc_minval("P_C_OF"), upper = rc_maxval("P_C_OF"), any.missing = TRUE)
  }else{
    checkmate::assert(all(c("P_DOSE","P_C_OF") %in% names(dt)),
                      msg = "Provide both P_DOSE and P_C_OF when B_C_OF_INPUT is absent")
    checkmate::assert_numeric(dt$P_DOSE, any.missing = FALSE)
    checkmate::assert_numeric(dt$P_C_OF, any.missing = FALSE)
  }

  # Create copy of data table
  dt.org <- copy(dt)
 
  # Add year and month of amendment application
  dt.org[, year := year(P_DATE_FERTILIZATION)]
  dt.org[, month := month(P_DATE_FERTILIZATION)]
 
  # add dpm-rpm ratio
  dt.org[,fr_dpm_rpm := fifelse(P_HC < 0.92, -2.174 * P_HC + 2.02, 0)]
  
  # add default ratio if no humification coefficient is supplied
  dt.org[is.na(fr_dpm_rpm), fr_dpm_rpm := 1.44]
  
  
  # estimate total Carbon input per crop and year (kg C/ha)
    if("B_C_OF_INPUT" %in% names(dt.org)){
      # If supplied, copy value, calculate from P_DOSE and P_C_OF only for NA rows
      dt.org[, cin_tot := B_C_OF_INPUT]
      if (anyNA(dt.org$B_C_OF_INPUT)) {
        dt.org[is.na(cin_tot), cin_tot := P_DOSE * P_C_OF/1000]
      }
  }else{
    # If not supplied calculate from dose (kg/ha) and organic carbon content (g C/kg)
  dt.org[, cin_tot := P_DOSE * P_C_OF/1000]
  }
  
  # estimate C input for DPM, RDM and HUM pool
  dt.org[, cin_hum := 0.02 * cin_tot]
  dt.org[, cin_dpm := (1 - 0.02) * cin_tot * fr_dpm_rpm/ (1 + fr_dpm_rpm)]
  dt.org[, cin_rpm := (1 - 0.02) * cin_tot - cin_dpm]
 
  # select only relevant columns
  dt.org <- dt.org[,list( year, month, cin_tot, cin_hum, cin_dpm, cin_rpm)]

  # return
  return(dt.org)
}
