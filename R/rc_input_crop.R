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
  M_GREEN_TIMING = M_CROPRESIDUE = M_IRRIGATION = M_RENEWAL = NULL
  CF_YIELD = YEAR = crft = B_LU = B_LU_EOM = fr_dpm_rpm = B_LU_HC = NULL
  cin_crop = cin_res= B_LU_EOM_RESIDUE = cin_crop_dpm = cin_crop_rpm = cin_res_dpm = cin_res_rpm = NULL
  
  # check B_LU_BRP or crop table
  checkmate::assert_integerish(B_LU_BRP, any.missing = FALSE, null.ok = TRUE, min.len = 1)
  checkmate::assert_subset(B_LU_BRP, choices = unique(rotsee::rc_crops$crop_code), empty.ok = TRUE)
  checkmate::assert_data_table(dt,null.ok = TRUE)
  checkmate::assert_subset(colnames(dt),choices = c("year","B_LU_EOM","B_LU_EOM_RESIDUE", "B_LU_HC","M_GREEN_TIMING","M_CROPRESIDUE","B_LU",'M_IRRIGATION','cf_yield'), empty.ok = TRUE)
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
  
  # Update EOM input from temporary grassland
  dt.crop[,crft := fifelse(grepl('nl_266',B_LU),1,0)]
  
  # Calculate consecutive temporary grassland years
  for(i in 1:nrow(dt.crop)){if(i == 1){dt.crop[,crft := 0 + crft]}else{dt.crop[i,crft := fifelse(crft == 0, 0,crft + dt.crop[i - 1, crft])]}}
  
  # Update B_LU_EOM based on grass age
  dt.crop[crft == 1, B_LU_EOM := 1175]
  dt.crop[crft == 2, B_LU_EOM := 2575]
  dt.crop[crft >= 3, B_LU_EOM := 3975]
  dt.crop[,crft := NULL]
  
  # add dpm-rmp ratio
  dt.crop[,fr_dpm_rpm := fifelse(B_LU_HC < 0.92, -2.174 * B_LU_HC + 2.02, 0)]
  
  # replace dpm-rmp ratio with defaults when information hc is missing
  dt.crop[is.na(fr_dpm_rpm), fr_dpm_rpm := 1.44]
  
  # estimate total Carbon input per crop and year (kg C / ha)
  dt.crop[, cin_crop := B_LU_EOM * 0.5 / B_LU_HC]
  dt.crop[, cin_res := B_LU_EOM_RESIDUE *0.5 / B_LU_HC]
  
  # estimate averaged C input for DPM, RDM and HUM pool (kg C / ha)
  dt.crop[,cin_crop_dpm := cin_crop * fr_dpm_rpm / (1 + fr_dpm_rpm)]
  dt.crop[,cin_crop_rpm := cin_crop * 1 / (1 + fr_dpm_rpm)]
  dt.crop[,cin_res_dpm := cin_res * fr_dpm_rpm / (1 + fr_dpm_rpm)]
  dt.crop[,cin_res_rpm := cin_res * 1 / (1 + fr_dpm_rpm)]
  
  # set residue to zero when residues are removed from the field
  dt.crop[M_CROPRESIDUE == FALSE,cin_res_dpm := 0]
  dt.crop[M_CROPRESIDUE == FALSE,cin_res_rpm := 0]
  
  # select only relevant columns with C input (kg C/ ha)
  dt.crop <- dt.crop[,list(year = YEAR,B_LU,cin_crop_dpm, cin_crop_rpm,cin_res_dpm,cin_res_rpm,
                           M_GREEN_TIMING,M_IRRIGATION,M_CROPRESIDUE,M_RENEWAL,
                           cf_yield = CF_YIELD)]
  
  # return
  return(dt.crop)
}
