#' Do check and expand amendment input table for RothC
#'
#' Helper function to check the content and format of the amendment input table.
#'
#' @param dt (data.table) Table with amendments and amendment properties for Carbon input.
#' @param B_LU_BRP (numeric) The crop code
#'
#' @details
#' The amendments table used as input for carbon modelling requires at minimum data on effective organic matter inputs and related year.
#' This helper function assists the checking and controlling of amendments properties involved.
#'
#' rothc_amendment: amendment table
#' Includes the columns:
#' * P_ID (character), ID of the soil amendment product
#' * P_NAME (character), name of the soil amendment product, optional
#' * P_C_OF_INPUT (numeric), the organic carbon input from soil amendment product on a field level (kg C/ha)
#' * P_DOSE (numeric), applied dose of soil amendment product (kg/ha), required if P_C_OF_INPUT is not supplied
#' * P_C_OF (numeric), organic carbon content of the soil amendment product (g C/kg), required if P_C_OF_INPUT is not supplied
#' * P_HC (numeric), the humification coefficient of the soil amendment product (fraction)
#' * P_DATE_FERTILIZATION (date), date of fertilizer application (formatted YYYY-MM-DD)
#'
#' @export
rc_input_amendment <- function(dt = NULL, B_LU_BRP = NULL){
  

  # add visual bindings
  P_C_OF = P_C_OF_INPUT = P_DATE_FERTILIZATION = P_DOSE = P_HC = NULL
  P_ID = P_NAME = cin_dpm = cin_hum = cin_rpm = cin_tot = fr_dpm_rpm = NULL
  
  # Check amendment table
  checkmate::assert_data_table(dt, null.ok = FALSE)
  checkmate::assert_subset(colnames(dt),choices = c("P_ID","P_NAME", "P_C_OF_INPUT", "P_DOSE", "P_C_OF", "P_HC", "P_DATE_FERTILIZATION"), empty.ok = TRUE)
  checkmate::assert_date(as.Date(dt$P_DATE_FERTILIZATION), any.missing = F)

  
  # Create copy of data table
  dt.org <- copy(dt)
 
  # Add year and month of amendment application
  dt.org[, year := year(P_DATE_FERTILIZATION)]
  dt.org[, month := month(P_DATE_FERTILIZATION)]
 
  # add month = NA when no input given
  if(!'month' %in% colnames(dt.org)){dt.org[,month := NA_real_]}
 
  # add dpm-rmp ratio
  dt.org[,fr_dpm_rpm := fifelse(P_HC < 0.92, -2.174 * P_HC + 2.02, 0)]
  
  # estimate total Carbon input per crop and year (kg C/ha)
  if(!is.null(dt.org$P_C_OF_INPUT)){
    # If supplied, copy value
    dt.org[, cin_tot := P_C_OF_INPUT]
  }else{
    # If not supplied calculate from dose (kg/ha) and organic carbon content (g C/kg)
  dt.org[, cin_tot := P_DOSE * P_C_OF/1000]
  }
  
  # estimate C input for DPM, RDM and HUM pool
  dt.org[, cin_hum := 0.02 * cin_tot]
  dt.org[, cin_dpm := (1 - 0.02) * cin_tot * fr_dpm_rpm/ (1 + fr_dpm_rpm)]
  dt.org[, cin_rpm := (1 - 0.02) * cin_tot - cin_dpm]
 
  # select only relevant columns
  dt.org <- dt.org[,list(p_ID = P_ID, p_name = P_NAME, year, month, cin_tot, cin_hum, cin_dpm, cin_rpm)]

  # return
  return(dt.org)
}
