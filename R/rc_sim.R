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
#' @param rothc_parms (list) A list with simulation parameters controlling the dynamics of RothC Model. Default is NULL. For more information, see details.
#'
#' @details
#' This function simulates the fate of SOC given the impact of soil properties, weather and management.
#' The following inputs are mandatory: rothc_rotation, A_SOM_LOI (\%), and A_CLAY_MI (\%). All other data is optional.
#' When no weather inputs are given, these are estimated from long-term average weather conditions in the Netherlands.
#'
#' The organic amendment table includes the columns year, month, P_NAME, P_DOSE, P_HC, P_OM, and p_p2o5, where month is optional.
#' P_NAME is the fertilizer name, P_DOSE has units (kg / ha), P_HC is the humification coefficient (fraction), P_OM is the organic matter content (%) and p_p2o5 is the phosphate content (%)
#'
#' The crop table includes the columns: year, B_LU (a crop id), B_LU_NAME (a crop name), B_LU_EOM_CROP (the effective organic matter content, kg/ha), B_LU_EOM_CROPRESIDUE (the effective organic matter content for crop residues, kg/ha), and the B_LU_HC (the humification coeffient,-).
#' The crop table might include the measures M_GREEN, M_GREEN_PROPERLY, M_CROPRESIDUE, M_IRRIGATION and M_RENEWAL, all in upper case.
#' * M_GREEN_TIMING (character) the month in which the catch crop is sown, options: (august,september,october,november,never)
#' * M_CROPRESIDUE (boolean) gives whether crop residues are amended to the soil after harvest.
#' * M_IRRIGATION (boolean) gives whether the crop is irrigated.
#' * M_RENEWAL (boolean) gives whether the grassland is renewed (only applicable for grassland)
#'
#' The simulation of C via the RothC model can be adapted by the following parameters: initialize, c_fractions, dec_rates, simyears and unit.
#' These have to be in a list called 'rothc_parms'.
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
                   rothc_rotation = NULL,
                   rothc_amendment = NULL,
                   rothc_parms = list(simyears = 50, initialize = FALSE,spinup = 10,method='adams')){
  
  # add visual bindings
  code = value_min = value_max = a_depth = dens.sand = dens.clay = cf = bd = toc = NULL
  b_depth = var = time = cf_abc = ciom.ini = biohum.ini = cbio.ini = chum.ini = CIOM0 = CDPM0 = CRPM0 = CBIO0 = CHUM0 = NULL
  soc = CDPM = CRPM = CBIO = CHUM = CIOM = bd = . = NULL
  
  # add internal table
  rcp <- rotsee::rc_parms
  
  # add checks
  checkmate::assert_numeric(A_SOM_LOI, lower = rcp[code == "A_SOM_LOI", value_min], upper = rcp[code == "A_SOM_LOI", value_max],len = 1)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(A_DEPTH, lower = 0, upper = 0.6, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(B_DEPTH, lower = 0, upper = 0.3, any.missing = FALSE, len = 1)
  checkmate::assert_numeric(cf_yield,lower = 0.1, upper = 2.0, any.missing = FALSE,len = 1)
  checkmate::assert_character(M_TILLAGE_SYSTEM, any.missing = FALSE,len=1)
  checkmate::assert_subset(M_TILLAGE_SYSTEM,choices = c('NT','ST','CT','DT'), empty.ok = FALSE)
  checkmate::assert_data_table(rothc_rotation)
  checkmate::assert_data_table(rothc_amendment)
  checkmate::assert_subset(colnames(rothc_rotation),choices = c("year","B_LU_EOM","B_LU_EOM_RESIDUE", "B_LU_HC","M_GREEN_TIMING","M_CROPRESIDUE","B_LU",'M_IRRIGATION','cf_yield'), empty.ok = FALSE)
  checkmate::assert_subset(colnames(rothc_amendment),choices = c("P_NAME", "year","month","P_OM","P_HC","p_p2o5", "P_DOSE"), empty.ok = FALSE)
  
  # create an internal crop rotation file
  dt.crop <- rc_input_crop(dt = rothc_rotation, cf_yield = cf_yield)
  
  # create an internal amendment file
  dt.org <- rc_input_amendment(dt = rothc_amendment)
  
  # rothC model parameters
  
  # add check on method solver
  if(is.null(rothc_parms$method)){method <- 'adams'} else {method <- 'adams'}
  
  # add check on initialise
  if(is.null(rothc_parms$initialize)){initialize <- TRUE} else {initialize <- rothc_parms$initialize}
  
  # add checks on simulation years
  if(is.null(rothc_parms$simyears)){simyears <- 50} else {simyears <- rothc_parms$simyears}
  if(!is.null(rothc_parms$spinup)){simyears <- simyears + rothc_parms$spinup}
  
  # add checks on units and time scale output for RothC (use defaults)
  if(is.null(rothc_parms$unit)){unit <- 'soc'} else {unit <- rothc_parms$unit}
  poutput <- 'year'
  
  # add checks on decomposition rates
  if(is.null(rothc_parms$dec_rates)){
    k1 = 10; k2 = 0.3; k3 = 0.66; k4 = 0.02
  } else {
    rcp <-  c(names(rothc_parms$dec_rates),colnames(rothc_parms$dec_rates))
    if('k1' %in% rcp){k1 <- rothc_parms$dec_rates[['k1']]}
    if('k2' %in% rcp){k2 <- rothc_parms$dec_rates[['k2']]}
    if('k3' %in% rcp){k3 <- rothc_parms$dec_rates[['k3']]}
    if('k4' %in% rcp){k4 <- rothc_parms$dec_rates[['k4']]}
  }
  
  # adapt decomposition rate when ploughing is reduced
  if('NT' %in% M_TILLAGE_SYSTEM){k4 = 0.8 * k4}
  if('ST' %in% M_TILLAGE_SYSTEM){k4 = 0.9 * k4}
  
  # add checks on C distribution over pools
  
  # set defaults equal to input
  c_fractions <- list(fr_IOM = 0.049,
                      fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015)
  
  # overwrite with user input
  if(!is.null(rothc_parms$c_fractions)){
    
    rcp <-  c(names(rothc_parms$c_fractions),colnames(rothc_parms$c_fractions))
    if('fr_IOM' %in% rcp){c_fractions$fr_IOM <- rothc_parms$c_fractions[['fr_IOM']]}
    if('fr_DPM' %in% rcp){c_fractions$fr_DPM <- rothc_parms$c_fractions[['fr_DPM']]}
    if('fr_RPM' %in% rcp){c_fractions$fr_RPM <- rothc_parms$c_fractions[['fr_RPM']]}
    if('fr_BIO' %in% rcp){c_fractions$fr_BIO <- rothc_parms$c_fractions[['fr_BIO']]}
    
  }
  
  
  # prepare the RothC model inputs
  
  # make rate modifying factors input database
  dt.rmf <- rc_input_rmf(dt = dt.crop,A_CLAY_MI = A_CLAY_MI, B_DEPTH = B_DEPTH,simyears = simyears, cf_yield = cf_yield)
  
  # combine RothC input parameters
  rothc.parms <- list(k1 = k1,k2 = k2, k3=k3, k4=k4, R1 = dt.rmf$R1, abc = dt.rmf$abc, d = dt.rmf$d)
  
  # prepare EVENT database with all C inputs over time 
  rothc.event <- rc_input_events(crops = dt.crop,amendment = dt.org,A_CLAY_MI = A_CLAY_MI,simyears = simyears)
  
  # initialize the RothC pools (kg C / ha)
  if(initialize == TRUE){ 
    
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
     
     # derive the initial distribution of C pools (original data.tables are used as input)
     cols <- c('fr_IOM','fr_DPM','fr_RPM','fr_BIO')
     dt.soc[,c(cols) := as.list(rc_initialise(crops = rothc_rotation, 
                                              amendment = rothc_amendment,
                                              B_LU_BRP,A_SOM_LOI,A_CLAY_MI,
                                              type ='spinup_analytical_bodemcoolstof'))]
     
    # Set the intial C pools (kg C / ha)
    dt.soc[,CIOM0 := toc * fr_IOM]
    dt.soc[,CDPM0 := toc * fr_DPM]
    dt.soc[,CRPM0 := toc * fr_RPM]
    dt.soc[,CBIO0 := toc * fr_BIO]
    dt.soc[,CHUM0 := toc * (1 - fr_IOM - fr_DPM - fr_RPM-fr_BIO)]
    
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