#' Function for initializing the RothC model
#'
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param A_SOM_LOI (numeric) value for the soil organic matter content of the soil
#' @param A_CLAY_MI (numeric) value for the clay content of the soil
#'
#' @import data.table
#'
#' @export
rc_initialise <- function(B_LU_BRP,A_SOM_LOI,A_CLAY_MI){
  
  # add visual bindings
  . = CIOM = CDPM = CRPM = CBIO = NULL
  
  # Prepare input for scenario Business As Usual
  scen.inp <- rc_input_scenario(B_LU_BRP = B_LU_BRP, scen = 'BAU')
  rotation <- scen.inp$rotation
  amendment <- scen.inp$amendment
  
  # Set model parameters
  parms <- list(simyears = 150,unit = 'psomperfraction', initialize = TRUE)
  
  # Run initialization run for 150 years
  this.result <- rc_sim(A_SOM_LOI = A_SOM_LOI,
                               A_CLAY_MI = A_CLAY_MI,
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