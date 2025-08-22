#' Simulate SOC evolution using Roth-C
#'
#' This function calculates the change in carbon stock or C pools (in kg C per ha) based on organic matter amendments, crop rotation, and long-term averaged weather conditions.
#'
#' @param A_SOM_LOI (numeric) The percentage organic matter in the soil (\%).
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param A_DEPTH (numeric) Depth for which soil sample is taken (m). Default set to 0.3.
#' @param B_DEPTH (numeric) Depth of the cultivated soil layer (m), simulation depth. Default set to 0.3.
#' @param M_TILLAGE_SYSTEM (character) gives the tillage system applied. Options include NT (no-till), ST (shallow-till), CT (conventional-till) and DT (deep-till).
#' @param cf_yield (numeric) A relative yield correction factor (fraction) if yield is higher than regional average
#' @param rothc_rotation (data.table) Table with crop rotation details and crop management actions that have been taken. Includes also crop inputs for carbon. See details for desired format.
#' @param rothc_amendment (data.table) A table with the following column names: year, month, P_NAME, P_DOSE, P_HC, P_OM, and p_p2o5, where month is optional.
#' @param rothc_parms (list) A list with simulation parameters controlling the dynamics of RothC Model. For more information, see details.
#' @param weather (data.table) Table with following column names: month, W_TEMP_MEAN_MONTH, W_PREC_MEAN_MONTH, W_ET_POT_MONTH, W_ET_ACT_MONTH. For more information, see details.
#'
#' @details
#' This function simulates the fate of SOC given the impact of soil properties, weather and management.
#' The following inputs are mandatory: rothc_rotation, A_SOM_LOI (\%), and A_CLAY_MI (\%). All other data is optional.
#' When no weather inputs are given, these are estimated from long-term average weather conditions in the Netherlands.
#'
#' rothc_amendment: amendment table
#' P_NAME is the fertilizer name, P_DOSE has units (kg / ha), P_HC is the humification coefficient (fraction), P_OM is the organic matter content (%) and p_p2o5 is the phosphate content (%)
#'
#' rothc_rotation: crop table
#' Includes the columns: year, B_LU (a crop id), B_LU_NAME (a crop name), B_LU_EOM_CROP (the effective organic matter content, kg/ha), B_LU_EOM_CROPRESIDUE (the effective organic matter content for crop residues, kg/ha), and the B_LU_HC (the humification coeffient,-).
#' May additionally include the columns M_GREEN_TIMING, M_CROPRESIDUE, M_IRRIGATION and M_RENEWAL, all in upper case.
#' * M_GREEN_TIMING (character) the month in which the catch crop is sown, options: (august,september,october,november,never)
#' * M_CROPRESIDUE (boolean) whether crop residues are amended to the soil after harvest.
#' * M_IRRIGATION (boolean) whether the crop is irrigated.
#' * M_RENEWAL (boolean) whether the grassland is renewed (only applicable for grassland)
#'
#' rothc_parms: simulation parameters
#' Table with possible parameters to adapt the calculations. May include the columns initialize, c_fractions, dec_rates, simyears and unit.
#' * initialize: scenario to initialize the carbon pools. Options TRUE or FALSE, default is FALSE
#' * c_fractions: Distribution over the different C pools.
#' * dec_rates: Decomposition rates of the different pools
#' * simyears: Duration of simulation (years), default is 50
#' * unit: Unit in which the output should be given. Options: 'A_SOM_LOI','psoc','cstock','psomperfraction','omb'
#' 
#' weather: Average weather conditions
#' Table containing columns month, W_TEMP_MEAN_MONTH (temperature in °C), W_PREC_MEAN_MONTH (precipitation in mm), W_ET_POT_MONTH (potential evapotranspiration in mm), and W_ET_ACT_MONTH. (actual evapotranspiration in mm).
#' If no table is given, average Dutch conditions are used
#'
#' @import deSolve
#'
#' @export
rc_sim <- function(A_SOM_LOI,
                   A_CLAY_MI,
                   A_DEPTH = 0.3,
                   B_DEPTH = 0.3,
                   cf_yield = 1,
                   M_TILLAGE_SYSTEM = 'CT',
                   rothc_rotation,
                   rothc_amendment = NULL,
                   rothc_parms = NULL,
                   weather = NULL){
  
  # add visual bindings
  code = value_min = value_max = a_depth = dens.sand = dens.clay = cf = bd = toc = NULL
  b_depth = var = time = cf_abc = ciom.ini = biohum.ini = cbio.ini = chum.ini = CIOM0 = CDPM0 = CRPM0 = CBIO0 = CHUM0 = NULL
  soc = dec_rates = simyears = c_fractions = method = poutput = unit = CDPM = CRPM = CBIO = CHUM = CIOM = bd =  . = NULL
  
  # add internal table
  rcp <- rotsee::rc_parms
  
  # Add missing data, check input (see rc_helpers)
  dt.weather <- rc_update_weather(dt = weather)
  rothc_parms <- rc_update_parms(parms = rothc_parms)
  
  # Unpack rothc_parms for often used parameters
  # Define decomposition rates
  k1 <- rothc_parms$dec_rates[["k1"]]
  k2 <- rothc_parms$dec_rates[["k2"]]
  k3 <- rothc_parms$dec_rates[["k3"]]
  k4 <- rothc_parms$dec_rates[["k4"]]
  
  # Define C fractions
  c_fractions <- rothc_parms$c_fractions
  
  # Define unit of output
  unit <- rothc_parms$unit
  
  # Define simyears
  simyears <- rothc_parms$simyears
  
  # Define method
  method <- rothc_parms$method
  
  # Define wanted output
  poutput <- rothc_parms$poutput
  
  # Define initialize
  initialize <- rothc_parms$initialize

  # add checks
  checkmate::assert_numeric(A_SOM_LOI, lower = rcp[code == "A_SOM_LOI", value_min], upper = rcp[code == "A_SOM_LOI", value_max],len = 1)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_DEPTH, lower = 0, upper = 0.6, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 0.3, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(cf_yield,lower = 0.1, upper = 2.0, any.missing = FALSE,len = 1)
  checkmate::assert_character(M_TILLAGE_SYSTEM, any.missing = FALSE,len=1)
  checkmate::assert_subset(M_TILLAGE_SYSTEM,choices = c('NT','ST','CT','DT'), empty.ok = FALSE)
  checkmate::assert_data_table(rothc_rotation)
  if(!is.null(rothc_amendment)){
    checkmate::assert_data_table(rothc_amendment)
  }
  checkmate::assert_names(colnames(rothc_rotation),must.include = c("year","B_LU_EOM","B_LU_EOM_RESIDUE", "B_LU_HC","B_LU", "B_LU_NAME"))
  if(!is.null(rothc_amendment)) {
    checkmate::assert_names(colnames(rothc_amendment),must.include = c("P_NAME", "year","P_OM","P_HC","p_p2o5", "P_DOSE"))
  }
  
  
  # create an internal crop rotation file
  dt.crop <- rc_input_crop(dt = rothc_rotation, cf_yield = cf_yield)
  
  # create an internal amendment file
  
  dt.org <- rc_input_amendment(dt = rothc_amendment)
  
  # rothC model parameters

  # prepare the RothC model inputs
  # make rate modifying factors input database
  dt.rmf <- rc_input_rmf(dt = dt.crop,A_CLAY_MI = A_CLAY_MI, B_DEPTH = B_DEPTH,simyears = simyears, cf_yield = cf_yield, dt.weather = dt.weather)
  
  # combine RothC input parameters
  rothc.parms <- list(k1 = k1,k2 = k2, k3=k3, k4=k4, R1 = dt.rmf$R1, abc = dt.rmf$abc, d = dt.rmf$d)
  
  # prepare EVENT database with all C inputs over time 
  rothc.event <- rc_input_events(crops = dt.crop,amendment = dt.org,A_CLAY_MI = A_CLAY_MI,simyears = simyears)
  
  # initialize the RothC pools (kg C / ha)
  
  # make internal data.table
  dt.soc <- data.table(A_SOM_LOI = A_SOM_LOI,A_CLAY_MI = A_CLAY_MI,a_depth = A_DEPTH,b_depth = B_DEPTH)
  
  # Correct A_SOM_LOI for sampling depth
  dt.soc[a_depth < 0.3 & A_CLAY_MI <= 10, A_SOM_LOI := A_SOM_LOI * (1 - 0.19 * ((0.20 - (pmax(0.10, a_depth) - 0.10))/ 0.20))]
  dt.soc[a_depth < 0.3 & A_CLAY_MI > 10, A_SOM_LOI := A_SOM_LOI * (1 - 0.33 * ((0.20 - (pmax(0.10, a_depth) - 0.10))/ 0.20))]
  
  # calculate soil texture dependent density
  dt.soc[, dens.sand := (1 / (0.02525 * A_SOM_LOI + 0.6541)) * 1000]
  dt.soc[, dens.clay :=  (0.00000067*A_SOM_LOI^4 - 0.00007792*A_SOM_LOI^3 + 0.00314712*A_SOM_LOI^2 - 0.06039523*A_SOM_LOI + 1.33932206) * 1000]
  
  # fraction clay correction
  dt.soc[, cf := pmin(1, A_CLAY_MI/25)]
  
  # clay dependent density
  dt.soc[, bd := cf * dens.clay + (1-cf) * dens.sand]
  
  # calculate total organic carbon (kg C / ha)
  dt.soc[,toc := A_SOM_LOI * 0.5 * bd * b_depth * 100 * 100 / 100]
  
  # set the default initialisation to the one used in BodemCoolstof
  if(initialize == TRUE){
    
    # set TOC to ton C / ha
    dt.soc[, toc := toc * 0.001]
    
    # time correction (is 12 in documentation Chantals' study, not clear why, probably due to fixed time step calculation)
    timecor = 1
    
    # set rate modifying parameters
    abc <- rothc.parms$abc
    
    # CDPM pool (ton C / ha)
    cdpm.ini <- rothc.event[var == 'CDPM',list(time,value)]
    cdpm.ini[,cf_abc := abc(time)]
    cdpm.ini <- cdpm.ini[,((sum(value) * 0.001 / max(time)) / (mean(cf_abc)/timecor))/k1]
    dt.soc[, cdpm.ini := mean(cdpm.ini)]
    
    # CRPM pool (ton C / ha)
    crpm.ini = rothc.event[var == 'CRPM',list(time,value)]
    crpm.ini[,cf_abc := abc(time)]
    crpm.ini <- crpm.ini[,((sum(value) * 0.001 / max(time)) / (mean(cf_abc)/timecor))/k2]
    dt.soc[, crpm.ini := mean(crpm.ini)]
    
    # CIOM pool (ton C / ha)
    dt.soc[, ciom.ini := 0.049 * toc^1.139]
    
    # CBIOHUM pool (ton C /ha)
    dt.soc[,biohum.ini := toc - ciom.ini - crpm.ini - cdpm.ini]
    
    # set to defaults when RPM and DPM inputs exceeds 70% / 50% of total C to avoid negative values for initial C pools
    dt.soc[biohum.ini <0, cdpm.ini := 0.015 * (toc-ciom.ini)]
    dt.soc[biohum.ini <0, crpm.ini := 0.125 * (toc-ciom.ini)]
    dt.soc[, biohum.ini := toc-ciom.ini - crpm.ini - cdpm.ini]
    
    # CBIO and CHUM pool
    dt.soc[,cbio.ini := biohum.ini / (1 + k3 / k4)]
    dt.soc[,chum.ini := biohum.ini / (1 + k4 / k3)]
    
    # Set the intial C pools (kg C / ha)
    dt.soc[,CIOM0 := ciom.ini * 1000]
    dt.soc[,CDPM0 := cdpm.ini * 1000]
    dt.soc[,CRPM0 := crpm.ini * 1000]
    dt.soc[,CBIO0 := cbio.ini * 1000]
    dt.soc[,CHUM0 := chum.ini * 1000]
    
  } else {
    
    # Calculate carbon pools based on default distribution (kg C / ha)
    dt.soc[,CIOM0 := c_fractions$fr_IOM * ((toc*0.001)^1.139) * 1000]
    dt.soc[,CDPM0 := c_fractions$fr_DPM * (toc-CIOM0)]
    dt.soc[,CRPM0 := c_fractions$fr_RPM * (toc-CIOM0)]
    dt.soc[,CBIO0 := c_fractions$fr_BIO * (toc-CIOM0)]
    dt.soc[,CHUM0 := toc-CIOM0-CDPM0-CRPM0-CBIO0]
    
  }
  
  # extract relevant columns
  rothc.ini <- dt.soc[,list(CIOM0,CDPM0,CRPM0,CBIO0,CHUM0)]
  
  
  # run RothC model
  
  # set time vector for RothC, add event times
  rothc.times <- seq(0,simyears,12/12)
  rothc.times <- c(rothc.event$time,rothc.times)
  rothc.times <- sort(unique(rothc.times))
  
  # set initial distribution of C pool
  y  = c(CDPM = rothc.ini$CDPM0,
         CRPM = rothc.ini$CRPM0,
         CBIO = rothc.ini$CBIO0,
         CHUM = rothc.ini$CHUM0)
  
  # set random seed
  set.seed(123)
  
  # run the model
  out <- deSolve::ode(y = y,
                      times = rothc.times,
                      rc_ode,
                      parms = rothc.parms,
                      events=list(data=rothc.event),
                      method = method,
                      rtol = 0.1,
                      atol = 1)
  # set to data.table
  out <- as.data.table(out)
  
  # estimate total SOC
  out[,soc := round(CDPM + CRPM + CBIO + CHUM + dt.soc$CIOM0)]
  
  # get only the SOC values on the time scale of years
  if(poutput=='year'){out <- out[time %in% 0:simyears]}
  
  # select type output
  if(unit=='soc') {
    
    # subset the RothC simulation result
    rothc.soc <- out[,list(year=time,soc)]
    
    # estimate bulk density
    rothc.soc[,bd := mean(dt.soc$bd)]
    rothc.soc[,A_CLAY_MI := mean(dt.soc$A_CLAY_MI)]
    rothc.soc[,A_DEPTH := A_DEPTH]
    
    # set C stocks back to OS%
    rothc.soc[,A_SOM_LOI := soc * 100 * 2 / (bd * B_DEPTH * 100 * 100)]
    
    # Correct A_SOM_LOI for sampling depth
    rothc.soc[A_DEPTH < 0.3 & A_CLAY_MI <= 10, A_SOM_LOI := A_SOM_LOI / (1 - 0.19 * ((0.20 - (pmax(0.10, A_DEPTH) - 0.10))/ 0.20))]
    rothc.soc[A_DEPTH < 0.3 & A_CLAY_MI > 10, A_SOM_LOI := A_SOM_LOI / (1 - 0.33 * ((0.20 - (pmax(0.10, A_DEPTH) - 0.10))/ 0.20))]
    
    # select output variables
    out <- rothc.soc[,list(year,A_SOM_LOI)]
    
  } else if (unit == 'psomperfraction'){
    
    # subset the RothC simulation result
    rothc.soc <- copy(out)
    
    # estimate bulk density
    rothc.soc[,bd := mean(dt.soc$bd)]
    rothc.soc[,A_CLAY_MI := mean(dt.soc$A_CLAY_MI)]
    rothc.soc[,A_DEPTH := A_DEPTH]
    rothc.soc[,CIOM := dt.soc$CIOM0]
    
    # do unit conversion for all pools, convert to %SOM
    cols <- c('soc','CDPM','CRPM','CBIO','CHUM','CIOM')
    rothc.soc[,c(cols) := lapply(.SD,function(x) x * 100 * 2 / (bd * B_DEPTH * 100 * 100)),.SDcols = cols]
    
    # Correct soc for sampling depth
    rothc.soc[A_DEPTH < 0.3 & A_CLAY_MI <= 10, soc := soc / (1 - 0.19 * ((0.20 - (pmax(0.10, A_DEPTH) - 0.10))/ 0.20))]
    rothc.soc[A_DEPTH < 0.3 & A_CLAY_MI > 10, soc := soc / (1 - 0.33 * ((0.20 - (pmax(0.10, A_DEPTH) - 0.10))/ 0.20))]
    
    # select output variables
    out <- rothc.soc[,.(year = time,A_SOM_LOI = soc,CDPM,CRPM,CBIO,CHUM,CIOM)]
    
  } else if (unit=='all'){
    
    rothc.soc <- out[,list(year = time,soc,CDPM,CRPM,CBIO,CHUM,CIOM = dt.soc$CIOM0)]
    
  }
  
  # update year
  # out[,year := year + rotation[1,year] - 1]
  
  # return output
  return(out)
}