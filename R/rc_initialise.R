#' Function for initializing the RothC model
#'
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param soil_properties (list) List with soil properties: A_C_OF, soil organic carbon content (g/kg) or B_C_ST03, soil organic carbon stock (Mg C/ha), preferably for soil depth 0.3 m; A_CLAY_MI, clay content (\%); A_DENSITY_SA, dry soil bulk density (g/cm3)
#'
#' @import data.table
#'
#' @export
rc_initialise <- function(B_LU_BRP,soil_properties){
  
  # add visual bindings
  . = CIOM = CDPM = CRPM = CBIO = A_SOM_LOI = NULL
  
  # Prepare input for scenario Business As Usual
  scen.inp <- rc_input_scenario(B_LU_BRP = B_LU_BRP, scen = 'BAU')
  rotation <- scen.inp$rotation
  amendment <- scen.inp$amendment
  
  # Set model parameters
  parms <- list(simyears = 150,unit = 'psomperfraction', initialize = TRUE)
  
  # Run initialization run for 150 years
  this.result <- rc_sim(soil_properties = soil_properties,
                               rothc_rotation = rotation,
                               rothc_amendment = amendment,
                               rothc_parms = parms)
  
  # take last two rotations
  this.result.fin <- this.result[year > max(year)-2*nrow(rotation),lapply(.SD,mean)]
  
  fractions <- this.result.fin[,.(fr_IOM = CIOM / (A_SOM_LOI),
                                  fr_DPM = CDPM / A_SOM_LOI,
                                  fr_RPM = CRPM / A_SOM_LOI,
                                  fr_BIO = CBIO / A_SOM_LOI)]
  
  
  # unlist fractions
  fractions <- unlist(fractions)
  
  # Return output
  return(fractions)
  
}