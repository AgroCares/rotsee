#' Function for initializing the RothC model
#'
#' @param crops (data.table) Table with crop rotation, cultivation management, year and potential Carbon inputs.
#' @param amendment (data.table) A table with the following column names: year, month, cin_tot, cin_hum, cin_dpm, cin_rpm and the fraction eoc over p (fr_eoc_p). Month is optional.
#' @param dt.soc (data.table) Data table containing information on soil properties. See details for information.
#' @param dt.time (data.table) Combination of months and years of the entire simulation period. 
#' @param rothc.parms (list) List with relevant RothC parameters. See details for more information
#' @param rothc.event (data.table) List with events of C inputs. See details for more information
#' @param start_date (character, formatted YYYY-MM-DD) start date of the simulation, required if type is set to spinup_simulation
#' @param soil_properties (list) list of relevant soil properties, required if type is set to spinup_simulation
#' @param dt.weather (data.table) average weather conditions for the location of interested, recommended if type is set to spinup_simulation
#' @param type (character) options for spin-up (spinup_simulation,spinup_analytical_bodemcoolstof, spinup_analytical_heuvelink)
#'
#'
#'
#' @import data.table
#' @importFrom stats weighted.mean
#' 
#' @details
#' #' soil_properties: soil properties table
#' Includes the columns:
#' * A_C_OF (numeric), soil organic carbon content (g C/kg), preferably for soil depth 0.3 m
#' * B_C_ST03 (numeric), soil organic carbon stock (Mg C/ha), preferably for soil depth 0.3 m. Required if A_C_OF is not supplied
#' * A_CLAY_MI (numeric), clay fraction (\%)
#' * A_DENSITY_SA (numeric), dry soil bulk density(g/cm3). In case this is not know, can be calculated using function \link{rc_calculate_bd} given a clay and organic matter content
#' 
#' amendment: amendment table. Input can be duplicated to cover the entire simulation period using \link{rc_extend_amendments}
#' Includes the columns:
#' * B_C_OF_INPUT (numeric), the organic carbon input from soil amendment product on a field level (kg C/ha)
#' * P_DOSE (numeric), applied dose of soil amendment product (kg/ha), required if B_C_OF_INPUT is not supplied
#' * P_C_OF (numeric), organic carbon content of the soil amendment product (g C/kg), required if B_C_OF_INPUT is not supplied
#' * P_HC (numeric), the humification coefficient of the soil amendment product (fraction)
#' * P_DATE_FERTILIZATION (date), date of fertilizer application (formatted YYYY-MM-DD)
#' 
#' crops: crop table. Input can be duplicated to cover the entire simulation period using \link{rc_extend_crops}
#' Includes the columns: 
#' * B_LU_START (start of crop rotation),
#' * B_LU_END (end of crop rotation),
#' * B_LU (a crop id), 
#' * B_LU_NAME (a crop name, optional),
#' * B_LU_HC, the humification coefficient of crop organic matter (-). When not supplied, default RothC value will be used
#' * B_C_OF_INPUT, the organic carbon input on field level (kg C/ha). In case not known, can be calculated using function \link{rc_calculate_B_C_OF}
#'
#' 
#' rothc.parms: list with RothC parameters.
#' Contains the following columns:
#' * k1 (numeric) decomposition rate of the DPM pool
#' * k2 (numeric) decomposition rate of the RPM pool
#' * k3 (numeric) decomposition rate of the BIO pool
#' * k4 (numeric) decomposition rate of the hum pool
#' * abc (function) function to calculate rate modifying factors as function of time
#' * R1 (numeric) Correction factor for soil structure
#' * time (list) list of the entire simulation period
#' 
#' rothc.events: data table with C input events
#' Contains the following columns:
#' * time (numeric) time within the simulation run, where 1 equals the first year
#' * var (character) C pool under consideration, options being 'CDPM', 'CRPM', and 'CHUM'.
#' * method (character) Method of considering listed value, default is add.
#' * value (numeric) carbon amount added (kg C/ha)
#' 
#' dt.soc: data table with soil information
#' contains at least the following columns:
#' * A_CLAY_MI (numeric), clay content (\%)
#' * toc (numeric), total carbon content (kg C / ha)
#'
#' weather: Weather table. If no table is given, average Dutch conditions are used
#' Includes the columns:
#' * month
#' * W_TEMP_MEAN_MONTH (temperature in °C)
#' * W_PREC_SUM_MONTH (precipitation in mm)
#' * W_ET_POT_MONTH (potential evapotranspiration in mm)
#' * W_ET_ACT_MONTH (actual evapotranspiration in mm)
#' 
#' Choice of initialisation type depends on data. spinup_simulation/spinup_analytical_bodemcoolstof assume equilibrium in C distribution between pools; 
#' spinup_analytical_Heuvelink assumes equilibrium in total C stocks. If C stocks are in equilibrium, the latter is preferable
#' Otherwise one of the other two is fine.
#'
#' @export
rc_initialise <- function(crops = NULL,
                          amendment = NULL,
                          dt.soc,
                          rothc.parms,
                          rothc.event,
                          dt.time = NULL,
                          start_date = NULL,
                          soil_properties = NULL,
                          dt.weather = NULL,
                          type ='spinup_analytical_bodemcoolstof'){
  
  # add visual bindings
  . = CIOM = CDPM = CRPM = CBIO = B_LU_EOM = M_CROPRESIDUE = B_LU_HC = chum.ini = NULL
  P_DOSE = P_OM = M_GREEN_TIMING = fr_dpm_rpm = P_HC = B_LU_EOM_RESIDUE = NULL
  abc = bd = time = toc = var = cf_abc = ciom.ini = biohum.ini = cbio.ini = NULL
  A_SOM_LOI = B_C_OF_INPUT = P_C_OF = NULL

  # Input validation by type
  if (type == 'spinup_simulation') {
    if (is.null(start_date)) stop("start_date is required for spinup_simulation")
    if (is.null(crops)) stop("crops is required for spinup_simulation")
    if (is.null(soil_properties)) stop("soil_properties is required for spinup_simulation")
  }
  
  if (type %in% c('spinup_analytical_heuvelink','spinup_analytical_bodemcoolstof')) {
    if (missing(dt.time) || is.null(dt.time)) stop("dt.time is required for analytical spin-up types")
    if (missing(dt.soc) || is.null(dt.soc)) stop("dt.soc is required for analytical spin-up types")
    if (is.null(dt.soc$A_CLAY_MI)) stop("dt.soc must contain A_CLAY_MI column")
    if (is.null(dt.soc$toc)) stop("dt.soc must contain toc column")
    }
  
  
  # Define rothc parameters
  k1 <- rothc.parms$k1
  k2 <- rothc.parms$k2
  k3 <- rothc.parms$k3
  k4 <- rothc.parms$k4
  
  abc <- rothc.parms$abc
  
  # initialise options
  
  ## do a simulation for 150 years to estimate the C fractions assuming system is in equilibrium
  if(type =='spinup_simulation'){
   
   # Extend crop and amendment files to include entire 150 year simulation
    crop_extend <- if(!is.null(crops)){
      rc_extend_crops(start_date = start_date, simyears = 150, crops = crops)
    }else{
      NULL
    }
    
    amendment_extend <- if(!is.null(amendment)){
      rc_extend_amendments(start_date = start_date, simyears = 150, amendments = amendment)
    }else{
      NULL
    }
 
    
    # Set model parameters
    parms <- list(unit = 'psomperfraction',
                  type = 'none')
    
    # Set newly required inputs
  
    # Run initialization run for 150 years
    this.result <- rc_sim(rothc_rotation = crop_extend,
                          rothc_amendment = amendment_extend,
                          soil_properties = soil_properties,
                          weather = dt.weather,
                          rothc_parms = parms)
    
    # take last two rotations
    this.result.fin <- this.result[year > max(year)-2*nrow(crops),lapply(.SD,mean)]
  
  
    fractions <- this.result.fin[,.(fr_IOM = CIOM / A_SOM_LOI,
                                    fr_DPM = CDPM / A_SOM_LOI,
                                    fr_RPM = CRPM / A_SOM_LOI,
                                    fr_BIO = CBIO / A_SOM_LOI)]
    
  }
  
  ## calculate initial carbon pools assuming equilibrium in C pools, using analytical solution (Heuvelink)
  if(type=='spinup_analytical_heuvelink'){

    # averaged total C input (kg C/ha/year) from crops and amendments
    if (is.null(crops) || nrow(crops) == 0) {
      c_input_crop <- 0
    }else{
    c_input_crop <- crops[,sum(B_C_OF_INPUT)/max(dt.time$time)]
    }
    
    if (is.null(amendment) || nrow(amendment) == 0) {
       c_input_man <- 0
    } else if (!is.null(amendment$B_C_OF_INPUT) && any(!is.na(amendment$B_C_OF_INPUT))) {
        c_input_man <- amendment[, sum(B_C_OF_INPUT)/max(dt.time$time)]
    } else {
          c_input_man <- amendment[, sum(P_DOSE * P_C_OF, na.rm = TRUE)/max(dt.time$time)]
    }
  

    # calculate C input ratio of crop and amendments
    c_input_tot <- c_input_crop+c_input_man
    
    if(c_input_tot <= 0 || !is.finite(c_input_tot)){
      # if there are no C inputs, continue calculation based on crops only to prevent NaNs
      CR_proportion <- 1
      M_proportion <- 0
    }else{
    CR_proportion <- c_input_crop / (c_input_tot)
    M_proportion <- c_input_man / (c_input_tot)
    }
    
    # estimate DPM-RPM ratio of the inputs from crop and amendments
    if (!is.null(crops) && nrow(crops) > 0) {
    crops[,fr_dpm_rpm := fifelse(B_LU_HC < 0.92, -2.174 * B_LU_HC + 2.02, 0)]
    }
    
    if (!is.null(amendment) && nrow(amendment) > 0) {
    amendment[,fr_dpm_rpm := fifelse(P_HC < 0.92, -2.174 * P_HC + 2.02, 0)]
    }
    
    # calculate average dpm_rpm ratio of C inputs from crops and amendments
    if (is.null(crops) || nrow(crops) == 0) {
      DR_crop <- 0
    } else {
      DR_crop <- crops[, weighted.mean(fr_dpm_rpm, w = B_C_OF_INPUT, na.rm = TRUE)]
      if (!is.finite(DR_crop)) DR_crop <- 0
    }
    
    
    if (is.null(amendment) || nrow(amendment) == 0) {
      DR_amendment <- 0  
    } else if (!is.null(amendment$B_C_OF_INPUT) && any(!is.na(amendment$B_C_OF_INPUT))) {
      DR_amendment <- amendment[P_DOSE > 0, weighted.mean(fr_dpm_rpm, w = B_C_OF_INPUT, na.rm = TRUE)]
    } else {
      DR_amendment <- amendment[P_DOSE > 0, weighted.mean(fr_dpm_rpm, w = (P_DOSE * P_C_OF), na.rm = TRUE)]
    }
    if (!is.finite(DR_amendment) || length(DR_amendment) == 0) DR_amendment <- 0
    
     # establish simyear based on time data table
    isimyears <- max(dt.time$year)
    
    # Define ratio CO2 / (BIO+HUM)
    x <- 1.67 * (1.85 + 1.60 * exp(-0.0786 * dt.soc$A_CLAY_MI))
    
    # transfer coefficients (B is BIO, H = HUM)
    B = 0.46/(x + 1)
    H = 0.54/(x + 1)
    
    # rates of the five pools, DPM, RPM, BIO, HUM and IOM 
    ks <- c(k1, k2, k3, k4, 0)
    
    # make C flow matrix based on k (removed) and transfer coefficients (added)
    A = diag(-ks)
    A[3, ] = A[3, ] + B * ks
    A[4, ] = A[4, ] + H * ks
 
    # Define average rate modifying factor over the crop rotation 
    xi <- mean(abc(1:(12*isimyears)/12))
    
    # Establish fraction of plant material ending in the DPM pool
    rho <- DR_crop / (1 + DR_crop)
    
    # Establish fractions of amendment ending in DPM (tau), RPM (nu), and HUM (2%) pool
    tau <- (1 - 0.02) * DR_amendment / (1 + DR_amendment)
    nu <- (1 - 0.02) * 1 / (1 + DR_amendment)
    
    # Recalculate total SOC stock to ton C/ha 
    CTOT <- dt.soc$toc / 1000
      
    # Calculate IOM pool (ton C / ha) based on Falloon method
    FallIOM <- 0.049 * CTOT^1.139
    
    # Calculate active SOC pool (ton C / ha)
    CTOT4 <- CTOT - FallIOM
    
    # vector where crop inputs ends up: rho % DPM, (1-rho) % RPM, 0% BIO, 0% HUM
    rho_vector <- matrix(c(rho, 1 - rho, 0, 0), nrow = 4, ncol = 1)
    
    # vector where amendment ends up: tau % DPM, nu % RPM, 0% BIO, 2% HUM
    tau_nu_vector <- matrix(c(tau, nu, 0, 1 - tau - nu), nrow = 4, ncol = 1)
    
    # Combined input vector to know fraction of total C inputs ending up in pools
    input_vector <- CR_proportion * rho_vector + M_proportion * tau_nu_vector
    
    ## calculate carbon inputs
    I4 <- matrix(c(1, 1, 1, 1), nrow = 1, ncol = 4) 
    A4 <- A[1:4, 1:4] # Input matrix without IOM flow

    # Check if input matrix is invertible
    if (det(A4) == 0) {
      stop("Matrix A4 is singular - check decomposition rates in rothc.parms")
    }
    
    # Invert A4
    A4inv <- tryCatch(
      solve(A4),
      error = function(e)stop("Failed to invert matrix A4: ", e$message)
    )
    
    # Calculate carbon inputs from amendments/crops
    I <- as.numeric(CTOT4 / (-(1 / xi) * I4 %*% A4inv %*% input_vector))
    
    # Check if I is finite and positive
    if (!is.finite(I) || I <= 0) {
      stop("spinup_Heuvelink initialisation produced invalid results. Check input data and parameters.")
      }
    
    # carbon pool calculation
    Q <- input_vector * I 
    C <- -(1 / xi) * A4inv %*% Q
  
    # calculate carbon pool at equilibrium state
    Cpools <- c(C, FallIOM)
    Ctotal <- sum(Cpools)
    
    fractions <- list(fr_IOM = Cpools[5] / Ctotal,
                      fr_DPM = Cpools[1] / Ctotal,
                      fr_RPM = Cpools[2] / Ctotal,
                      fr_BIO = Cpools[3] / Ctotal)
  }
  
  ## calculate initial C pools assuming equilibrium in C stocks, using analytical solution (as implemented in BodemCoolstof tool)
  if(type=='spinup_analytical_bodemcoolstof'){

    # create local copy
    dt.soc <- copy(dt.soc)
    
    # recalculate total organic carbon from kg to ton C / ha
    dt.soc[,toc := toc/1000]
    
    # time correction (is 12 in documentation Chantals' study, not clear why, probably due to fixed time step calculation)
    timecor = 1

    # CDPM pool (ton C / ha)
    cdpm.ini <- rothc.event[var == 'CDPM',list(time,value)]
    if(nrow(cdpm.ini) >0){
    cdpm.ini[,cf_abc := abc(time)]
    cdpm.ini <- cdpm.ini[,((sum(value) * 0.001 / max(time)) / (mean(cf_abc)/timecor))/k1]
    dt.soc[, cdpm.ini := mean(cdpm.ini)]
    }else{
      dt.soc[,cdpm.ini := 0]
    }
    
    # CRPM pool (ton C / ha)
    crpm.ini = rothc.event[var == 'CRPM',list(time,value)]
    if(nrow(crpm.ini) > 0) {
    crpm.ini[,cf_abc := abc(time)]
    crpm.ini <- crpm.ini[,((sum(value) * 0.001 / max(time)) / (mean(cf_abc)/timecor))/k2]
    dt.soc[, crpm.ini := mean(crpm.ini)]
    }else{
      dt.soc[,crpm.ini := 0]
    }
    
    # CIOM pool (ton C / ha)
    dt.soc[, ciom.ini := 0.049 * toc^1.139]
    
    # CBIOHUM pool (ton C /ha)
    dt.soc[,biohum.ini := toc - ciom.ini - crpm.ini - cdpm.ini]
    
    # set to defaults (see RothC documentation) when RPM and DPM inputs exceeds 70% / 50% of total C to avoid negative values for initial C pools
    dt.soc[biohum.ini <0, cdpm.ini := 0.015 * (toc-ciom.ini)]
    dt.soc[biohum.ini <0, crpm.ini := 0.125 * (toc-ciom.ini)]
    dt.soc[, biohum.ini := toc-ciom.ini - crpm.ini - cdpm.ini]
    
    # CBIO and CHUM pool
    dt.soc[,cbio.ini := biohum.ini / (1 + k3 / k4)]
    dt.soc[,chum.ini := biohum.ini / (1 + k4 / k3)]
 
    # define fractions
    fractions <- dt.soc[,.(
      fr_IOM = fifelse(toc >0, ciom.ini / toc, 0),
      fr_DPM = fifelse(toc >0, cdpm.ini / toc, 0),
      fr_RPM = fifelse(toc >0, crpm.ini / toc, 0),
      fr_BIO = fifelse(toc >0, cbio.ini / toc, 0))]
    
  }
  
  # unlist fractions
  fractions <- unlist(fractions)
  
  # Return output
  return(fractions)
  
}