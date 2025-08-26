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
#' * year (date), year of fertilizer application
#' * month (numeric), month of fertilizer application, optional
#' * P_DATE_FERTILIZATION (date), date of fertilizer application (formatted YYYY-MM-DD)
#' To run this function, the dt requires as input:"P_NAME", "year","month","P_OM","P_HC","p_p2o5", and "P_DOSE"
#' if dt is NULL, then the amendment input will be prepared using function \link{rc_input_scenario} using scenario 'BAU'
#'
#' @export
rc_input_amendment <- function(dt = NULL, B_LU_BRP = NULL){
  

  # add visual bindings
  fr_dpm_rpm = P_HC = cin_tot = P_DOSE = P_OM = cin_hum = cin_dpm = P_NAME = p_p2o5 = cin_rpm = NULL
  
  # check B_LU_BRP or crop table
  checkmate::assert_data_table(dt, null.ok = TRUE)
  checkmate::assert_subset(colnames(dt),choices = c("P_ID","P_NAME", "P_C_OF_INPUT", "P_DOSE", "P_C_OF", "P_HC", "year", "month", "P_OM", "p_p2o5"), empty.ok = TRUE)
  if(!is.null(dt$month)){checkmate::assert_integerish(dt$month, lower = 1, upper = 12, any.missing = TRUE)}
  checkmate::assert_numeric(dt$p_p2o5, lower = 0, any.missing = FALSE)
  checkmate::assert_true(all(dt$p_p2o5 >0))
  
  checkmate::assert_integerish(B_LU_BRP, any.missing = FALSE, null.ok = TRUE, min.len = 1)
  checkmate::assert_subset(B_LU_BRP, choices = unique(rotsee::rc_crops$crop_code), empty.ok = TRUE)
  checkmate::assert_true(!is.null(dt) || !is.null(B_LU_BRP))

  
  # Create copy of data table, or create default based on Dutch scenario
  if(is.null(dt) & !is.null(B_LU_BRP)){
    
    rs <- rc_input_scenario(B_LU_BRP = B_LU_BRP, scen = 'BAU')
    dt.org <- rs$amendment
  } else {
    dt.org <- copy(dt)
  }
  
  # Set years to 1:x
  dt.org[,year := year - min(year) + 1]
  
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
  dt.org <- dt.org[,list(p_name = P_NAME, year, month, cin_tot, cin_hum, cin_dpm, cin_rpm, fr_eoc_p = P_OM * P_HC * 0.5 / p_p2o5)]
  
  # return
  return(dt.org)
}
