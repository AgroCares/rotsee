#' Helper function to weight and correct the risk and scores
#'
#' @param x The risk or score value to be weighted
#'
#' @examples
#' cf_ind_importance(x = 0.5)
#' cf_ind_importance(x = c(0.1,0.5,1.5))
#'
#' @return
#' A transformed variable after applying a inverse weighing function so that lower values will gain more impact when applied in a weighed.mean function. A numeric value.
#'
#' @export
cf_ind_importance <- function(x) {
  y <- 1 / (x  + 0.2)
  
  return(y)
}


#' Function to check user weather table and, if not supplied, insert default weather
#'
#' @param dt (data.table) Monthly weather table with the following columns:
#' *month (1 - 12)
#' *W_TEMP_MEAN_MONTH (°C)
#' *W_PREC_MEAN_MONTH (mm)
#' *W_ET_POT_MONTH (mm)
#' *W_ET_ACT_MONTH (mm; can be NA, will be derived based on W_ET_POT_MONTH)
#' If not supplied, default monthly weather based on the Netherlands is added
#' 
#' @returns
#' A data table containing monthly data of temperature, precipitation, and evapotranspiration.
#' 
#' @export
#'
rc_update_weather <- function(dt = NULL){
  if(!is.null(dt)){
    # Create weather data table
    dt <- copy(dt)
    
    # Check inputs
    checkmate::assert_data_table(dt, nrows = 12)
    checkmate::assert_subset(c("month", "W_TEMP_MEAN_MONTH", "W_PREC_MEAN_MONTH"), colnames(dt))
    checkmate::assert(
      any(c("W_ET_POT_MONTH", "W_ET_ACT_MONTH") %in% names(dt)),
      msg = "At least one of 'W_ET_POT_MONTH' or 'W_ET_ACT_MONTH' must be provided."
    )
    checkmate::assert_subset(dt$month, 1:12, add = FALSE)
    checkmate::assert_numeric(dt$W_TEMP_MEAN_MONTH, lower = -30, upper = 50, any.missing = FALSE, len = 12)
    checkmate::assert_numeric(dt$W_PREC_MEAN_MONTH, lower = 0, upper = 10000, any.missing = FALSE, len = 12)
    
    # Check if both potential and actual ET are provided
    if ("W_ET_POT_MONTH" %in% colnames(dt) && "W_ET_ACT_MONTH" %in% colnames(dt)) {
      checkmate::assert(
        !all(is.na(dt$W_ET_POT_MONTH)) || !all(is.na(dt$W_ET_ACT_MONTH)),
        msg = "At least one of W_ET_POT_MONTH and W_ET_ACT_MONTH should not contain NA values."
      )
      # Check ranges, allow NAs
      checkmate::assertNumeric(dt$W_ET_POT_MONTH, lower = 0, upper = 1000, any.missing = TRUE)
      checkmate::assertNumeric(dt$W_ET_ACT_MONTH, lower = 0, upper = 10000, any.missing = TRUE)
    } else if ("W_ET_POT_MONTH" %in% colnames(dt)) {
      # Only potential ET provided: no NA allowed
      checkmate::assertNumeric(dt$W_ET_POT_MONTH, lower = 0, upper = 1000, any.missing = FALSE)
    } else if ("W_ET_ACT_MONTH" %in% colnames(dt)) {
      # Only actual ET provided: no NA allowed
      checkmate::assertNumeric(dt$W_ET_ACT_MONTH, lower = 0, upper = 10000, any.missing = FALSE)
    }
    
}else{
  #Set default weather for Dutch conditions
  dt <- data.table(month = 1:12,
                   W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                   W_PREC_MEAN_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                   W_ET_POT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                   W_ET_ACT_MONTH = NA_real_)
}
  
  return(dt)
}



#' Function to check user given RothC simulation parameters, or provide default if none are given
#'
#' @param parms (list) List containing the columns dec_rates, c_fractions, initialize, simyears, unit, method, and poutput
#' 
#' @returns
#' A data table containing parameters to run the RothC simulation, with columns dec_rates, c_fractions, initialize, simyears, unit, method and poutput
#' 
#' 
#' @export
#'
#' 
rc_update_parms <- function(parms = NULL){
  
  
  # Add visible bindings
  c_fractions = NULL
  
  # Checks names parms
  if(!is.null(parms)){
    checkmate::assert_subset(colnames(parms), choices = c("dec_rates", "c_fractions", "initialize", "simyears", "unit", "method", "poutput"), empty.ok = TRUE)
    checkmate::assert_data_table(parms)
  }
  
  # add checks on decomposition rates
  # Define defaults decomposition rates
  k1 = 10; k2 = 0.3; k3 = 0.66; k4 = 0.02
  
  # if dec_rates supplied, check inputs and overwrite defaults
  if(!is.null(parms$dec_rates)){
    # check supplied decomposition rates
    checkmate::assert_numeric(parms$dec_rates, lower = 0, upper = 30, min.len = 1,max.len = 4)
    checkmate::assert_subset(names(parms$dec_rates),choices = c("k1", "k2", "k3", "k4"),empty.ok = TRUE)
    
    # Use supplied decomposition rates
    rcp <-  c(names(parms$dec_rates),colnames(parms$dec_rates))
    if('k1' %in% rcp){k1 <- parms$dec_rates[['k1']]}
    if('k2' %in% rcp){k2 <- parms$dec_rates[['k2']]}
    if('k3' %in% rcp){k3 <- parms$dec_rates[['k3']]}
    if('k4' %in% rcp){k4 <- parms$dec_rates[['k4']]}
  } 
  


  # add checks on c_fractions
  # define default c-fractions
  fr_IOM = 0.049; fr_DPM = 0.015; fr_RPM = 0.125; fr_BIO = 0.015
  
  # if c_fractions supplied, check input and overwrite defaults
  if(!is.null(parms$c_fractions)){
    # check inputs: initial C distribution over pools
    checkmate::assert_numeric(parms$c_fractions, lower = 0, upper = 1, any.missing = FALSE, len = 4,null.ok = FALSE)
    checkmate::assert_subset(names(parms$c_fractions),choices = c("fr_IOM", "fr_DPM", "fr_RPM", "fr_BIO"),empty.ok = TRUE)
    
    # Use supplied distribution
    rcp <-  c(names(parms$c_fractions),colnames(parms$c_fractions))
    if('fr_IOM' %in% rcp){fr_IOM <- parms$c_fractions[['fr_IOM']]}
    if('fr_DPM' %in% rcp){fr_DPM <- parms$c_fractions[['fr_DPM']]}
    if('fr_RPM' %in% rcp){fr_RPM <- parms$c_fractions[['fr_RPM']]}
    if('fr_BIO' %in% rcp){fr_BIO <- parms$c_fractions[['fr_BIO']]}
  }
  
  # add checks on initialise
  initialize <- TRUE
  
  if(!is.null(parms$initialize)){
    # check initialize
    checkmate::assert_logical(initialize,any.missing = FALSE, len = 1)
    
    initialize = parms$initialize
    
   
  }
  
  # add checks on simyears
  simyears <- 50
  
  if(!is.null(parms$simyears)){
    # check input for the number of simulation year
    checkmate::assert_integerish(parms$simyears, lower = 1)
    
    simyears = parms$simyears
    
  }
  
  # Add checks on unit
  unit <- "A_SOM_LOI"
  
  if(!is.null(parms$unit)){
      # check output format
    checkmate::assert_subset(unit,c('A_SOM_LOI','psoc','cstock','psomperfraction','omb'),empty.ok = FALSE)
    checkmate::assert_character(unit,len=1)
    
    unit = parms$unit
    
  }
  
  
  # add checks on method
  method <- "adams"
  if(!is.null(parms$method)){
    # check supplied method
    
    # define method
    method = parms$method
  }
  
  # add checks on poutput
  poutput <- 'year'
  if(!is.null(parms$poutput)){
    # check supplied poutput
    checkmate::assert_subset(poutput, c('year'), empty.ok = FALSE)
    checkmate::assert_character(poutput, len=1)
    
    poutput = parms$poutput
  }
  
  out = list(initialize = initialize,
             dec_rates = c(k1 = k1, k2 = k2, k3 = k3, k4 = k4),
             c_fractions = c(fr_IOM = fr_IOM, fr_DPM = fr_DPM, fr_RPM = fr_RPM, fr_BIO = fr_BIO),
             simyears = simyears,
             unit = unit,
             method = method,
             poutput = poutput)
  
  # return output
  return(out)
}

#' Function to check input tables of soil, crop, and amendment data
#'
#' @param soil_properties (list) List with soil properties: A_C_OF, soil organic carbon content (g/kg) or B_C_ST03, soil organic carbon stock (Mg C/ha), preferably for soil depth 0.3 m; A_CLAY_MI, clay content (\%); A_DENSITY_SA, dry soil bulk density (g/cm3)
#' @param rothc_rotation (data.table) Table with crop rotation details and crop management actions that have been taken. Includes also crop inputs for carbon. See details for desired format.
#' @param rothc_amendment (data.table) A table with the following column names: P_DATE_FERTILIZATION, P_ID, P_NAME, P_DOSE, P_C_OF, B_C_OF_INPUT, and P_HC.
#'
#' @returns
#' Error messages indicating if input data is not in order
#' 
#' @export
#'
rc_check_inputs <- function(soil_properties,
                            rothc_rotation,
                            rothc_amendment){
  
  # Add visual bindings
  
  # Check soil properties
  checkmate::assert_list(soil_properties, min.len = 3)
  if(length(soil_properties$A_C_OF) != 0)  checkmate::assert_numeric(soil_properties$A_C_OF, lower = 0.1, upper = 600, any.missing = FALSE, len = 1)
  if(length(soil_properties$B_C_ST03) != 0)  checkmate::assert_numeric(soil_properties$B_C_ST03, lower = 0.1, upper = 3000, any.missing = FALSE, len = 1)
  if((length(soil_properties$A_C_OF) == 0 || is.na(soil_properties$A_C_OF)) &&
     (length(soil_properties$B_C_ST03) == 0 || is.na(soil_properties$B_C_ST03))){
       stop('Both A_C_OF and B_C_ST03 are missing in soil_properties')}
  checkmate::assert_numeric(soil_properties$A_CLAY_MI, lower = 0.1, upper = 75, len = 1)
  checkmate::assert_numeric(soil_properties$A_DENSITY_SA, lower = 0.5, upper = 3, len = 1)
  
  # Check crop properties if supplied
  if(!is.null(rothc_rotation)){
    checkmate::assert_data_table(rothc_rotation, null.ok = TRUE, min.rows = 1)
    
    allowed <- c("year","month","B_LU","B_LU_NAME","B_LU_HC","P_C_OF","B_C_OF_INPUT",
                 "B_LU_YIELD","B_LU_DM","B_LU_HI","B_LU_HI_RES","B_LU_RS_FR",
                 "M_GREEN_TIMING","M_CROPRESIDUE","M_IRRIGATION","M_RENEWAL")
    checkmate::assert_subset(names(rothc_rotation), choices = allowed, empty.ok = FALSE)
    
    req <- c("year","B_LU","B_C_OF_INPUT")
    checkmate::assert_true(all(req %in% names(rothc_rotation)))
    
    checkmate::assert_numeric(rothc_rotation$B_LU_HC, lower = 0, upper = 1, any.missing = FALSE)
    checkmate::assert_numeric(rothc_rotation$B_C_OF_INPUT, lower = 0, upper = 15000, any.missing = FALSE)
    if ("B_LU_NAME" %in% names(rothc_rotation))
      checkmate::assert_character(rothc_rotation$B_LU_NAME, any.missing = FALSE)
    }
  
  # Check amendment properties if supplied
  if(!is.null(rothc_amendment)){
    checkmate::assert_data_table(rothc_amendment, null.ok = TRUE, min.rows = 1)
    
    allowed <- c("P_ID","P_NAME","B_C_OF_INPUT","P_DOSE","P_C_OF","P_HC","P_DATE_FERTILIZATION","month")
    checkmate::assert_subset(names(rothc_amendment), choices = allowed, empty.ok = FALSE)
    
    checkmate::assert_true("P_DATE_FERTILIZATION" %in% names(rothc_amendment))
    checkmate::assert_date(as.Date(rothc_amendment$P_DATE_FERTILIZATION), any.missing = FALSE)
    if ("P_NAME" %in% names(rothc_amendment))
      checkmate::assert_character(rothc_amendment$P_NAME, any.missing = TRUE)
    if ("P_DOSE" %in% names(rothc_amendment))
       checkmate::assert_numeric(rothc_amendment$P_DOSE, lower = 0, upper = 250000, any.missing = FALSE)
    if ("P_C_OF" %in% names(rothc_amendment))
      checkmate::assert_numeric(rothc_amendment$P_C_OF, lower = 0, upper = 1000, any.missing = FALSE)
    if ("B_C_OF_INPUT" %in% names(rothc_amendment))
      checkmate::assert_numeric(rothc_amendment$B_C_OF_INPUT, lower = 0, upper = 250000, any.missing = FALSE)
    checkmate::assert_true("P_HC" %in% names(rothc_amendment))
    checkmate::assert_numeric(rothc_amendment$P_HC, lower = 0, upper = 1, any.missing = FALSE)
    
  }
}


#' Function to calculate the dry soil bulk density based on Dutch pedotransfer functions
#'
#' @param dt (data table) Contains the columns A_CLAY_MI (clay content \%) and at least one of A_SOM_LOI (organic matter content, \%) and A_C_OF (organic carbon content, g C/kg)
#'
#' @returns
#' Data table with the dry soil bulk density (g/cm3)
#' 
#' @export
rc_calculate_bd <- function(dt){
  # add visible bindings
  dens.sand = A_SOM_LOI = A_C_OF = dens.clay = cf = A_CLAY_MI = A_DENSITY_SA = NULL

  # Check input data
  checkmate::assert_subset(colnames(dt), choices = c("A_SOM_LOI", "A_CLAY_MI", "A_C_OF"))
  if(is.null(dt$A_SOM_LOI)){
    checkmate::assert_numeric(dt$A_C_OF, lower = 0.1, upper = 600, any.missing = F)
  }else{
  checkmate::assert_numeric(dt$A_SOM_LOI, lower = 0.5, upper = 75, any.missing = F)
  }
  checkmate::assert_numeric(dt$A_CLAY_MI, lower = 0.1, upper = 75, any.missing = F)
  
  # Add copy of data table
  dt <- copy(dt)
  
  # calculate soil texture dependent density (CBAV, 2019)
  if(!is.null(dt$A_SOM_LOI)){
  dt[, dens.sand := (1 / (0.02525 * A_SOM_LOI + 0.6541))]
  dt[, dens.clay :=  (0.00000067*A_SOM_LOI^4 - 0.00007792*A_SOM_LOI^3 + 0.00314712*A_SOM_LOI^2 - 0.06039523*A_SOM_LOI + 1.33932206)]
  }else{
    dt[, dens.sand := (1 / (0.02525 * (A_C_OF*2/10) + 0.6541))]
    dt[, dens.clay :=  (0.00000067*(A_C_OF*2/10)^4 - 0.00007792*(A_C_OF*2/10)^3 + 0.00314712*(A_C_OF*2/10)^2 - 0.06039523*(A_C_OF*2/10) + 1.33932206)]
  }
  
  # fraction clay correction
  dt[, cf := pmin(1, A_CLAY_MI/25)]
  
  # clay dependent density
  dt[, A_DENSITY_SA := cf * dens.clay + (1-cf) * dens.sand]
  return(dt)
}

#' Function to calculate the C input of your crop
#'
#' @param dt (data table) Table with crop rotation details and crop management actions that have been taken. For more information see details.
#'
#' @returns
#' Data table with total C input from crops
#' 
#' @details
#' Crop data table
#' Contains the following columns:
#' * B_LU_YIELD (numeric), the mean crop yield (kg dry matter/ha)
#' * B_LU_HI (numeric), the harvest index of the crop
#' * B_LU_HI_RES (numeric), fraction of biomass that is residue
#' * B_LU_RS_FR (numeric), Root-to-shoot ratio of the crop
#' * M_CROP_RESIDUE (logical), indicator of whether crop residue is incorporated into the soil
#' 
#' @export

rc_calculate_B_C_OF <- function(dt){
  # Add visible bindings
  cin_aboveground = B_LU_YIELD = B_LU_HI = cin_roots = B_LU_RS_FR = NULL
  cin_residue = M_CROPRESIDUE = B_LU_HI_RES = B_C_OF_INPUT = NULL
  
  # Check input data
  checkmate::assert_subset(colnames(dt), choices = c("B_LU_YIELD", "B_LU_HI", "B_LU_HI_RES", "B_LU_RS_FR", "M_CROPRESIDUE"))
  checkmate::assert_numeric(dt$B_LU_YIELD, lower = 0, upper = 150000, any.missing = F)
  checkmate::assert_numeric(dt$B_LU_HI, lower = 0.01, upper = 1, any.missing = F)
  checkmate::assert_numeric(dt$B_LU_HI_RES, lower = 0, upper = 1, any.missing = F)
  checkmate::assert_numeric(dt$B_LU_RS_FR, lower = 0.01, upper = 5, any.missing = F)
  checkmate::assert_logical(dt$M_CROPRESIDUE)
  
  # Make copy of input table
  dt.crop <- copy(dt)
  
  # Calculate C inputs from roots and crop residue (kg C/ha)
  dt.crop[, cin_aboveground := B_LU_YIELD / B_LU_HI * 0.5]
  dt.crop[, cin_roots := cin_aboveground * B_LU_RS_FR]
  dt.crop[, cin_residue := fifelse(M_CROPRESIDUE, cin_aboveground * B_LU_HI_RES, 0)]
  dt.crop[, B_C_OF_INPUT := cin_roots + cin_residue]
  
return(dt.crop)
}