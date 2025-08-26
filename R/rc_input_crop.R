#' Do check and expand crop input table for RothC for the Netherlands
#'
#' Helper function to check the content and format of the crop input table.
#'
#' @param dt (data.table) Table with crop rotation and related crop properties for Carbon input.
#' @param B_LU_BRP (numeric) The crop code
#' @param cf_yield (numeric) A yield correction factor (fraction) if yield is higher than regional average
#'
#' @details
#' The crop table used as input for carbon modelling requires at minimum data on effective organic matter inputs and related year.
#' This helper function assists the checking and controlling of crop properties involved.
#'
#' To run this function, the dt requires as input: B_LU (a crop id), B_LU_NAME (a crop name, optional), B_LU_EOM (the effective organic matter content, kg/ha), B_LU_EOM_RESIDUE (the effective organic matter content for crop residues, kg/ha), and the B_LU_HC (the humification coeffient,-).
#' if dt is NULL, then the crop input will be prepared using function \link{rc_input_scenario} using scenario 'BAU'
#'
#' @export
rc_input_crop <- function(dt = NULL,B_LU_BRP = NULL,cf_yield){
  
  # add visual bindings
  M_GREEN_TIMING = M_CROPRESIDUE = M_IRRIGATION = M_RENEWAL = cin_dpm = B_C_OF_INPUT = cin_rpm = NULL
  CF_YIELD = YEAR = crft = B_LU  = fr_dpm_rpm = B_LU_HC = B_LU_HI_RES = NULL
  cin_aboveground = B_LU_YIELD =B_LU_DM = B_LU_HI = cin_roots = B_LU_RS_FR = cin_residue = NULL
  
  # check B_LU_BRP or crop table
  checkmate::assert_integerish(B_LU_BRP, any.missing = FALSE, null.ok = TRUE, min.len = 1)
  checkmate::assert_subset(B_LU_BRP, choices = unique(rotsee::rc_crops$crop_code), empty.ok = TRUE)
  checkmate::assert_data_table(dt,null.ok = TRUE)
  checkmate::assert_subset(colnames(dt),choices = c("year","B_LU_EOM_CROP","B_LU_EOM_CROPRESIDUE", "B_LU_HC","M_GREEN_TIMING","M_CROPRESIDUE","B_LU", "B_LU_NAME", "B_C_OF_INPUT","B_LU_YIELD", "B_LU_DM", "B_LU_HI", "B_LU_HI_RES", "B_LU_RS_FR"), empty.ok = TRUE)
  checkmate::assert_true(!(is.null(dt) & is.null(B_LU_BRP)))
  checkmate::assert_numeric(cf_yield,lower = 0.1, upper = 2.0, any.missing = FALSE,len = 1)
  
  # set default crop table in case that dt is missing
  if(is.null(dt) & !is.null(B_LU_BRP)){
    
    rs <- rc_input_scenario(B_LU_BRP = B_LU_BRP, scen = 'BAU')
    dt.crop <- rs$rotation
  } else {
    dt.crop  <- copy(dt)
  }
  
  # update crop basic properties
  if(!'M_GREEN_TIMING' %in% colnames(dt.crop)){dt.crop[,M_GREEN_TIMING := 'never']}
  if(!'M_CROPRESIDUE' %in% colnames(dt.crop)){dt.crop[,M_CROPRESIDUE := FALSE]}
  if(!'M_IRRIGATION' %in% colnames(dt.crop)){dt.crop[,M_IRRIGATION := FALSE]}
  if(!'M_RENEWAL' %in% colnames(dt.crop)){dt.crop[,M_RENEWAL := FALSE]}
  if(!'CF_YIELD' %in% colnames(dt.crop)){dt.crop[,CF_YIELD := cf_yield[1]]}
  
  # ensure that year always start with 1 to X, and sort
  dt.crop[,YEAR := year - min(year) + 1]
  setorder(dt.crop,YEAR)
  
  # add dpm-rmp ratio
  dt.crop[,fr_dpm_rpm := fifelse(B_LU_HC < 0.92, -2.174 * B_LU_HC + 2.02, 0)]
  
  # replace dpm-rmp ratio with defaults when information hc is missing
  dt.crop[is.na(fr_dpm_rpm), fr_dpm_rpm := 1.44]
  
  # estimate total Carbon input per crop and year (kg C / ha)
  # Add carbon inputs based on B_C_OF_INPUT
  if(!is.null(dt.crop$B_C_OF_INPUT)){
    
    # Estimate averaged C input for DPM and RDM pool (kg C / ha)
    dt.crop[,cin_dpm := B_C_OF_INPUT * fr_dpm_rpm / (1 + fr_dpm_rpm)]
    dt.crop[,cin_rpm := B_C_OF_INPUT * 1 / (1 + fr_dpm_rpm)]
    
  }else{
    # When B_C_OF_INPUT is missing estimate C input from yield
    # Calculate C inputs from roots and crop residue
    dt.crop[, cin_aboveground := B_LU_YIELD * B_LU_DM * 0.001 / B_LU_HI * 0.5]
    dt.crop[, cin_roots := cin_aboveground * B_LU_RS_FR]
    dt.crop[, cin_residue := fifelse(M_CROPRESIDUE, cin_aboveground * B_LU_HI_RES, 0)]
    
    # Estimate averaged C input for DPM and RDM pool (kg C / ha)
    dt.crop[,cin_dpm := (cin_roots + cin_residue) * fr_dpm_rpm / (1 + fr_dpm_rpm)]
    dt.crop[,cin_rpm := (cin_roots + cin_residue) * 1 / (1 + fr_dpm_rpm)]
  }
  

  # select only relevant columns with C input (kg C/ ha)
  dt.crop <- dt.crop[,list(year = YEAR,B_LU,cin_dpm, cin_rpm,
                           M_GREEN_TIMING,M_IRRIGATION,M_CROPRESIDUE,M_RENEWAL,
                           cf_yield = CF_YIELD)]
  
  # return
  return(dt.crop)
}
