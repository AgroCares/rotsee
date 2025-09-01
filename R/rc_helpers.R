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
#' *W_PREC_SUM_MONTH (mm)
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
    checkmate::assert_subset(c("month", "W_TEMP_MEAN_MONTH", "W_PREC_SUM_MONTH"), colnames(dt))
    checkmate::assert(
      any(c("W_ET_POT_MONTH", "W_ET_ACT_MONTH") %in% names(dt)),
      msg = "At least one of 'W_ET_POT_MONTH' or 'W_ET_ACT_MONTH' must be provided."
    )
    checkmate::assert_subset(dt$month, 1:12, add = FALSE)
    checkmate::assert_numeric(dt$W_TEMP_MEAN_MONTH, lower = -30, upper = 50, any.missing = FALSE, len = 12)
    checkmate::assert_numeric(dt$W_PREC_SUM_MONTH, lower = 0, upper = 10000, any.missing = FALSE, len = 12)
    
    # Check if both potential and actual ET are provided
    if ("W_ET_POT_MONTH" %in% colnames(dt) && "W_ET_ACT_MONTH" %in% colnames(dt)) {
      checkmate::assert(
        !all(is.na(dt$W_ET_POT_MONTH)) || !all(is.na(dt$W_ET_act_MONTH)),
        msg = "At least one of W_ET_POT_MONTH and W_ET_ACT_MONTH should not contain NA values."
      )
      # Check ranges, allow NAs
      checkmate::assertNumeric(dt$W_ET_POT_MONTH, lower = 0, upper = 1000, any.missing = TRUE)
      checkmate::assertNumeric(dt$W_ET_ACT_MONTH, lower = 0, upper = 10000, any.missing = TRUE)
    } else if ("W_ET_POT_MONTH" %in% colnames(dt)) {
      # Only potential ET provided: no NA allowed
      checkmate::assertNumeric(dt$W_ET_POT_MONTH, lower = 0, upper = 1000, any.missing = FALSE)
    } else if ("W_ET_act_MONTH" %in% colnames(dt)) {
      # Only actual ET provided: no NA allowed
      checkmate::assertNumeric(dt$W_ET_ACT_MONTH, lower = 0, upper = 10000, any.missing = FALSE)
    }
    
}else{
  #Set default weather for Dutch conditions
  dt <- data.table(month = 1:12,
                   W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                   W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                   W_ET_POT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                   W_ET_ACT_MONTH = NA_real_)
}
  
  return(dt)
}



#' Function to check user given RothC simulation parameters, or provide default if none are given
#'
#' @param parms (list) List containing the columns dec_rates, c_fractions, initialize, unit, method, poutput, start_date, end_date
#' @param crops (data.table) Data table with crop rotation information. Should at least contain the columns B_LU_START (YYYY-MM-DD) and B_LU_END (YYYY-MM-DD)
#' @param amendments (data.table) Data table with amendment input information. Should at least contain the column P_DATE_FERTILIZATION (YYYY-MM-DD)
#' 
#' @returns
#' A data table containing parameters to run the RothC simulation, with columns dec_rates, c_fractions, initialize, simyears, unit, method and poutput
#' 
#' 
#' @export
#'
#' 
rc_update_parms <- function(parms = NULL, crops = NULL, amendments = NULL){
  
 
  # Add visible bindings
  c_fractions = start_date = end_date = NULL
  
  # Checks names parms
  if(!is.null(parms)){
    checkmate::assert_subset(colnames(parms), choices = c("dec_rates", "c_fractions", "initialize", "unit", "method", "poutput", "start_date", "end_date"), empty.ok = TRUE)
    checkmate::assert_list(parms)
  }
  
  # add checks on decomposition rates
  # Define default decomposition rates
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
    checkmate::assert_logical(parms$initialize,any.missing = FALSE, len = 1)
    
    initialize = parms$initialize
    
   
  }
  
  # Check the format of start_date and end_date
  # Set dates in case start_date or end_date is not supplied
  dates <- data.table(date = c(crops$B_LU_START,crops$B_LU_END,amendments$P_DATE_FERTILIZATION))
  dates[, year := year(date)][, month := month(date)]
  setorder(dates,year,month)
  
  start_date <- dates[1, date]
  end_date <- dates[nrow(dates), date]
  
  if(!is.null(parms$start_date)){
    # check start_date 
    checkmate::assert_date(as.Date(start_date))
  
    start_date <- parms$start_date
  }
  
  if(!is.null(parms$end_date)){
    checkmate::assert_date(as.Date(end_date)) 
    
    end_date <- parms$end_date
  }

  if(as.Date(start_date) > as.Date(end_date)) stop('Start_date is not before end_date')
  
  
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
             unit = unit,
             method = method,
             poutput = poutput,
             start_date = start_date,
             end_date = end_date)
  
  # return output
  return(out)
}

#' Function to check input tables of soil, crop, and amendment data
#'
#' @param soil_properties (list) List with soil properties: A_C_OF, soil organic carbon content (g/kg) or B_C_ST03, soil organic carbon stock (Mg C/ha), preferably for soil depth 0.3 m; A_CLAY_MI, clay content (\%); A_DENSITY_SA, dry soil bulk density (g/cm3)
#' @param rothc_rotation (data.table) Table with crop rotation details and crop management actions that have been taken. Includes also crop inputs for carbon. See details for desired format.
#' @param rothc_amendment (data.table) A table with the following column names: P_DATE_FERTILIZATION, P_ID, P_NAME, P_DOSE, P_C_OF, P_C_OF_INPUT, and P_HC.
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
  if(length(soil_properties$A_C_OF) != 0)  checkmate::assert_numeric(soil_properties$A_C_OF, lower = 0.1, upper = 600, any.missing = TRUE, len = 1)
  if(length(soil_properties$B_C_ST03) != 0)  checkmate::assert_numeric(soil_properties$B_C_ST03, lower = 0.1, upper = 3000, any.missing = TRUE, len = 1)
  if(all(length(soil_properties$A_C_OF) == 0,length(soil_properties$B_C_ST03) == 0)) stop('Both A_C_OF and B_C_ST03 are missing in soil_properties')
  checkmate::assert_numeric(soil_properties$A_CLAY_MI, lower = 0.1, upper = 75, len = 1)
  checkmate::assert_numeric(soil_properties$A_DENSITY_SA, lower = 0.5, upper = 3, len = 1)
  
  # Check crop properties if supplied
  if(!is.null(rothc_rotation)){
  checkmate::assert_data_table(rothc_rotation,null.ok = TRUE)
  checkmate::assert_subset(colnames(rothc_rotation),choices = c("B_LU_START", "B_LU_END", "B_LU", "B_LU_NAME", "B_LU_HC","P_C_OF", "B_C_OF_INPUT", "B_LU_YIELD", "B_LU_DM", "B_LU_HI", "B_LU_HI_RES", "B_LU_RS_FR", "M_GREEN_TIMING","M_CROPRESIDUE", "M_IRRIGATION", "M_RENEWAL"), empty.ok = TRUE)
  checkmate::assert_character(rothc_rotation$B_LU_NAME, any.missing = F)
  checkmate::assert_numeric(rothc_rotation$B_LU_HC, lower = 0, upper = 1, any.missing = F)
  checkmate::assert_numeric(rothc_rotation$B_C_OF_INPUT, lower = 0, upper = 15000, any.missing = F)
  checkmate::assert_date(as.Date(rothc_rotation$B_LU_START), any.missing = F)
  checkmate::assert_date(as.Date(rothc_rotation$B_LU_END), any.missing = F)
    }
  
  # Check amendment properties if supplied
  if(!is.null(rothc_amendment)){
    checkmate::assert_data_table(rothc_amendment, null.ok = TRUE)
    checkmate::assert_subset(colnames(rothc_amendment),choices = c("P_ID","P_NAME", "P_C_OF_INPUT", "P_DOSE", "P_C_OF", "P_HC", "P_DATE_FERTILIZATION"), empty.ok = TRUE)
    checkmate::assert_character(rothc_amendment$P_NAME, any.missing = T)
    checkmate::assert_numeric(rothc_amendment$P_DOSE, lower = 0, upper = 250000, any.missing = F)
    checkmate::assert_numeric(rothc_amendment$P_C_OF, lower = 0, upper = 1000, any.missing = F)
    checkmate::assert_numeric(rothc_amendment$P_HC, lower = 0, upper = 1, any.missing = F)
    checkmate::assert_date(as.Date(rothc_amendment$P_DATE_FERTILIZATION))
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
  
  # calculate soil texture dependent density (BCAV 2019?)
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
#' * B_LU_HI_RES (numeric), fraction of biomass that is harvested
#' * B_LU_RS_FR (numeric), fraction of biomass that remains as residue
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
  checkmate::assert_numeric(dt$B_LU_DM, lower = 0, upper = 1000, any.missing = F)
  checkmate::assert_numeric(dt$B_LU_HI, lower = 0.01, upper = 1, any.missing = F)
  checkmate::assert_numeric(dt$B_LU_HI_RES, lower = 0, upper = 1, any.missing = F)
  checkmate::assert_numeric(dt$B_LU_RS_FR, lower = 0.01, upper = 1, any.missing = F)
  checkmate::assert_logical(dt$M_CROPRESIDUE)
  
  # Make copy of input table
  dt.crop <- copy(dt)
  
  # Calculate C inputs from roots and crop residue (kg C/ha)
  dt.crop[, cin_aboveground := B_LU_YIELD * 0.001 / B_LU_HI * 0.5]
  dt.crop[, cin_roots := cin_aboveground * B_LU_RS_FR]
  dt.crop[, cin_residue := fifelse(M_CROPRESIDUE, cin_aboveground * B_LU_HI_RES, 0)]
  dt.crop[, B_C_OF_INPUT := cin_roots + cin_residue]
  
return(dt.crop)
}



#' Function to extend the user crop input file to cover the full range of simulation years
#'
#' @param crops (data table) Input crop table for the RothC model. See details for further information
#' @param simyears (numeric) number of simulation years of the RothC model
#' @param start_date (character, formatted YYYY-MM-DD) Date in which simulation starts
#' @param end_date (character, formatted YYYY-MM-DD) Required if simyears is not supplied
#'
#' @returns
#' An extended crop input file to be used in the rtosee package
#' 
#' @details
#' Crops: crop table
#' Includes the columns: 
#' * B_LU_START (start of crop rotation),
#' * B_LU_END (end of crop rotation),
#' * B_LU (a crop id), 
#' * B_LU_NAME (a crop name, optional),
#' * B_LU_HC, the humification coefficient of crop organic matter (-). When not supplied, default RothC value will be used
#' * B_C_OF_INPUT, the organic carbon input on field level (kg C/ha)
#' 
#' 
#' @export
#'
rc_extend_crops <- function(crops,start_date, end_date = NULL, simyears = NULL){
  # add visible bindings
  id = B_LU_START = B_LU_END = yr_rep = year_start = year_end = NULL
  year_start_ext = year_end_ext = NULL
  
  # Check input data
  checkmate::assert_data_table(crops,null.ok = TRUE)
  checkmate::assert_subset(colnames(crops),choices = c("B_LU_START", "B_LU_END", "B_LU", "B_LU_NAME", "B_LU_HC","P_C_OF", "B_C_OF_INPUT", "B_LU_YIELD", "B_LU_DM", "B_LU_HI", "B_LU_HI_RES", "B_LU_RS_FR", "M_GREEN_TIMING","M_CROPRESIDUE", "M_IRRIGATION", "M_RENEWAL"), empty.ok = TRUE)
  checkmate::assert_character(crops$B_LU_NAME, any.missing = F)
  checkmate::assert_numeric(crops$B_LU_HC, lower = 0, upper = 1, any.missing = F)
  checkmate::assert_numeric(crops$B_C_OF_INPUT, lower = 0, upper = 15000, any.missing = F)
  checkmate::assert_date(as.Date(crops$B_LU_START), any.missing = F)
  checkmate::assert_date(as.Date(crops$B_LU_END), any.missing = F)
  checkmate::assert_date(as.Date(start_date))
  if(!is.null(end_date)){checkmate::assert_date(as.Date(start_date))}
  if(!is.null(simyears)){checkmate::assert_numeric(simyears, lower = 1)}
  if(is.null(end_date) && is.null(simyears)) stop('both end_date and simyears are missing in the input')
  
  # Copy crops table
  crops <- as.data.table(crops)
  setnames(crops,toupper(colnames(crops)))
  
  # Define total length of crop rotation (years)
  rotation_length <- max(year(crops$B_LU_END)) - year(start_date) + 1
  
  # Add an unique ID
  crops[,id := .I]
  
  # Define simyears if not supplied
  if(is.null(simyears)){
    simyears <- year(end_date) + month(end_date)/12 - (year(start_date) + month(start_date)/12)
  }
  
  # Determine number of duplications
  duplications <- ceiling(simyears / rotation_length)
  
  # Extend crop table for the number of years
  crops_ext <- crops[rep(id, each = duplications)]
 
  # Update  B_LU_START and B_LU_END for all repetitions of rotation block
  # Define start year for each repetition
  crops_ext[,`:=` (year_start = year(B_LU_START), year_end = year(B_LU_END))]
  
  # create helper column to identify years of repetition
  crops_ext[,yr_rep := 1:.N, by = id]
  
  # Recalculate start years based on length of crop rotation and number of repetitions
  crops_ext[,`:=` (year_start_ext = year_start + (yr_rep - 1) * ceiling(rotation_length),
                      year_end_ext = year_end + (yr_rep - 1) * ceiling(rotation_length))]
 
  # Update B_LU_START and B_LU_END
  crops_ext[, `:=` (B_LU_START = paste0(year_start_ext,substr(B_LU_START, start = 5, stop = 10)),
                       B_LU_END = paste0(year_end_ext,substr(B_LU_END, start = 5, stop = 10)))]

  # filter only the years for simulation
  this.crops <- crops_ext[year_start_ext <= (year(start_date) + simyears),]
 
  # Select relevant columns
  this.crops <- this.crops[, .SD, .SDcols =!names(this.crops) %in% c("id", "year_start", "year_end",
                                                       "yr_rep", "year_start_ext", "year_end_ext")
                                 ]
  
  # order crop file
  setorder(this.crops, B_LU_START)

  return(this.crops)
  
}
  


#' Function to extend the user amendments input file to cover the full range of simulation years
#'
#' @param amendments (data table) Input amendments table for the RothC model. See details for further information
#' @param simyears (numeric) number of simulation years of the RothC model
#' @param start_date (character, formatted YYYY-MM-DD) Date in which simulation starts
#' @param end_date (character, formatted YYYY-MM-DD) Required if simyears is not supplied
#'
#' @returns
#' An extended amendment input file to be used in the rotsee package
#' 
#' @details
#' amendments: amendment table
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
#'
rc_extend_amendments <- function(amendments,start_date, end_date = NULL, simyears = NULL){
  
  # Add visible bindings
  id = yr_rep = P_DATE_FERTILIZATION = NULL
  
  # Check input data
  checkmate::assert_data_table(amendments, null.ok = TRUE)
  checkmate::assert_subset(colnames(amendments),choices = c("P_ID","P_NAME", "P_C_OF_INPUT", "P_DOSE", "P_C_OF", "P_HC", "P_DATE_FERTILIZATION"), empty.ok = TRUE)
  checkmate::assert_character(amendments$P_NAME, any.missing = T)
  checkmate::assert_numeric(amendments$P_DOSE, lower = 0, upper = 250000, any.missing = F)
  checkmate::assert_numeric(amendments$P_C_OF, lower = 0, upper = 1000, any.missing = F)
  checkmate::assert_numeric(amendments$P_HC, lower = 0, upper = 1, any.missing = F)
  checkmate::assert_date(as.Date(amendments$P_DATE_FERTILIZATION))
  if(!is.null(end_date)){checkmate::assert_date(as.Date(start_date))}
  if(!is.null(simyears)){checkmate::assert_numeric(simyears, lower = 1)}
  if(is.null(end_date) && is.null(simyears)) stop('both end_date and simyears are missing in the input')
  
  # Make copy of amendments table
    amendments <- as.data.table(amendments)
    setnames(amendments,toupper(colnames(amendments)))
    
  # Define total length of amendment rotation (years)
  rotation_length <- max(year(amendments$P_DATE_FERTILIZATION)) - year(start_date) + 1
    
  # Add an unique ID
  amendments[,id := .I]
  
  # Define simyears if not supplied
  if(is.null(simyears)){
    simyears <- year(end_date) + month(end_date)/12 - (year(start_date) + month(start_date)/12)
  }
  
  # Determine number of duplications
  duplications <- ceiling(simyears / rotation_length)
    
  # Extend amendment table for the number of years
  amendments_ext <- amendments[rep(id, each = ceiling(simyears / rotation_length))]
    
  # Update P_DATE_FERTILIZATION for all repetitions of rotation block
  amendments_ext[, yr_rep := 1:.N, by = id]
  amendments_ext[, year := year(P_DATE_FERTILIZATION) + (yr_rep - 1) * rotation_length]
  amendments_ext[, P_DATE_FERTILIZATION := paste0(year,substr(P_DATE_FERTILIZATION, start = 5, stop = 10))]
    
  # filter only the years for simulation
  this.amendments <- amendments_ext[year <= (year(start_date) + simyears),]
    
  # Select relevant columns
  this.amendments <- this.amendments[, .SD, .SDcols =!names(this.amendments) %in% 
                                         c("id", "year", "year_start", "year_end",
                                           "yr_rep", "year_start_ext", "year_end_ext")
    ]

  # order amendments file
  setorder(this.amendments, P_DATE_FERTILIZATION)
  
  #return amendments file 
  return(this.amendments)
}



#' Function to create data table with dates and months of the entire simulation period
#'
#' @param start_date (date) start date of the simulation period (formatted as YYYY-MM-DD)
#' @param end_date (date) end date of the simulation period (formatted as YYYY-MM-DD)
#'
#' @returns
#' Table with all year and month combinations of the simulation period
#' @export
#'
rc_time_period <- function(start_date, end_date){
  #add visible bindings
  time = NULL
  
  # perform inputs checks
  checkmate::assert_date(as.Date(start_date))
  checkmate::assert_date(as.Date(end_date))
  
  # Create a complete set of year-month combinations for the simulation period
  dt.time <- CJ(year = min(year(start_date)):max(year(end_date)), month = 1:12)
  
  # Make selection of dates between start and end date
  dt.time <-  dt.time[,date := as.Date(paste(year, month, "01", sep = "-"))]
  
  dt.time <- dt.time[date >= as.Date(paste(year(start_date), month(start_date), "01", sep = "-")) &
                       date <= as.Date(paste(year(end_date), month(end_date), "01", sep = "-"))]
  dt.time[,date := NULL]
  
  # Format time
  dt.time[, time := .I / 12 - 1/12]
  
  #return output
  return(dt.time)
}
