#' Function to initialise the RothC model for a single field
#'
#' @param crops (data.table) Table with crop rotation, cultivation management, year and potential Carbon inputs.
#' @param amendment (data.table) A table with the following column names: P_DATE_FERTILIZATION and B_C_OF_INPUT or both P_DOSE and P_HC.
#' @param dt.soc (data.table) Data table containing information on soil properties. See details for information.
#' @param dt.time (data.table) Combination of months and years of the entire simulation period. 
#' @param rothc.parms (list) List with relevant RothC parameters. See details for more information
#' @param rothc.event (data.table) List with events of C inputs. See details for more information
#' @param start_date (character, formatted YYYY-MM-DD) start date of the simulation, required if initialisation_method is set to spinup_simulation
#' @param soil_properties (list) list of relevant soil properties, required if initialisation_method is set to spinup_simulation
#' @param dt.weather (data.table) average weather conditions for the location of interested, recommended if initialisation_method is set to spinup_simulation
#' @param initialisation_method (character) options for spin-up (spinup_simulation,spinup_analytical_bodemcoolstof, spinup_analytical_heuvelink)
#'
#'
#'
#' @import data.table
#' @importFrom stats weighted.mean
#' 
#' @details
#' soil_properties: soil properties table
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
#' * abcd (function) function to calculate rate modifying factors as function of time
#' * R1 (numeric) Correction factor for soil structure
#' * time (list) list of the entire simulation period
#' 
#' rothc.event: data table with C input events
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
#' * W_ET_REF_MONTH (potential evapotranspiration in mm)
#' * W_ET_ACT_MONTH (actual evapotranspiration in mm)
#' 
#' Choice of initialisation method depends on data. spinup_simulation/spinup_analytical_bodemcoolstof assume equilibrium in C distribution between pools; 
#' 
#' spinup_analytical_heuvelink assumes equilibrium in total C stocks. If C stocks are in equilibrium, the latter is preferable
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
                          initialisation_method ='spinup_analytical_bodemcoolstof'){
  
  # add visual bindings
  . = CIOM = CDPM = CRPM = CBIO = B_LU_EOM = M_CROPRESIDUE = B_LU_HC = chum.ini = NULL
  P_DOSE = P_OM = M_GREEN_TIMING = fr_dpm_rpm = P_HC = B_LU_EOM_RESIDUE = NULL
  abcd = bd = time = toc = var = cf_abcd = ciom.ini = biohum.ini = cbio.ini = NULL
  A_SOM_LOI = B_C_OF_INPUT = P_C_OF = NULL

  # check if a correct initialization method is supplied
  checkmate::assert_choice(initialisation_method, choices = c(
    'spinup_analytical_bodemcoolstof',
    'spinup_analytical_heuvelink',
    'spinup_simulation',
    'none'))
  
  # Input validation by type
  if (initialisation_method == 'spinup_simulation') {
    if (is.null(start_date)) stop("start_date is required for spinup_simulation")
    if (is.null(crops)) stop("crops is required for spinup_simulation")
    if (nrow(crops) == 0) stop("crops must contain at least one row for spinup_simulation")
    if (is.null(soil_properties)) stop("soil_properties is required for spinup_simulation")
  }
  
  if (initialisation_method %in% c('spinup_analytical_heuvelink','spinup_analytical_bodemcoolstof')) {
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
  
  abcd <- rothc.parms$abcd
  
  # initialise options
  
  ## do a simulation for 150 years to estimate the C fractions assuming system is in equilibrium
  if(initialisation_method =='spinup_simulation'){
   
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
                  initialisation_method = 'none',
                  dec_rates = c(k1 = k1, k2 = k2, k3 = k3, k4 = k4))
    
  
    # Run initialisation run for 150 years
    this.result <- rc_sim(rothc_rotation = crop_extend,
                          rothc_amendment = amendment_extend,
                          soil_properties = soil_properties,
                          weather = dt.weather,
                          rothc_parms = parms)
    
    # take last two rotations
    this.result.fin <- this.result[year > max(year)-2*nrow(crops),lapply(.SD,mean)]
  
  
    cpools <- this.result.fin[,.(CIOM0 = CIOM,
                                 CDPM0 = CDPM,
                                 CRPM0 = CRPM,
                                 CBIO0 = CBIO)]
    
  }
  
  ## calculate initial carbon pools assuming equilibrium in C pools, using analytical solution (Heuvelink)
  if(initialisation_method == 'spinup_analytical_heuvelink'){

    # establish simyears based on time data table
    isimyears <- max(dt.time$year) - min(dt.time$year) + 1
    
    # Estimate crop input dose and properties if supplied
    if (is.null(crops) || nrow(crops) == 0) {
      c_input_crop <- 0
      
      # set average DPM-RPM ratio of crops to 0
      DR_crop <- 0
      
    }else{
    # Average total C input (kg C/ha/yr)
    c_input_crop <- crops[,sum(B_C_OF_INPUT, na.rm = TRUE)/isimyears]
    
    # estimate DPM-RPM ratio of crop inputs
    crops[,fr_dpm_rpm := fifelse(B_LU_HC < 0.92, -2.174 * B_LU_HC + 2.02, 0)]
    
    # calculate average DPM-RPM ratio
    DR_crop <- crops[, weighted.mean(fr_dpm_rpm, w = B_C_OF_INPUT, na.rm = TRUE)]
    
    if (!is.finite(DR_crop)) DR_crop <- 0
    }
    
    
    # Estimate amendment input dose and properties
    if (is.null(amendment) || nrow(amendment) == 0) {
      # set amendment input to 0 if none supplied
       c_input_man <- 0
       
       # set average DPM-RPM ratio of amendment to 0
       DR_amendment <- 0  
       
    } else if (!is.null(amendment$B_C_OF_INPUT)) {
      # set amendment C input [kg C/ha]
        c_input_man <- amendment[, sum(B_C_OF_INPUT, na.rm = TRUE)/isimyears]
        
      # Estimate DPM-RPM ratio of amendment inputs
        amendment[,fr_dpm_rpm := fifelse(P_HC < 0.92, -2.174 * P_HC + 2.02, 0)]
        
        # Calculate average DPM-RPM ratio of amendment inputs
        DR_amendment <- amendment[B_C_OF_INPUT > 0, weighted.mean(fr_dpm_rpm, w = B_C_OF_INPUT, na.rm = TRUE)]
        
    } else {
          # calculate amendment inputs [kg C/ha]
          c_input_man <- amendment[, sum(P_DOSE * P_C_OF, na.rm = TRUE)/isimyears]
          
          # Estimate DPM-RPM ratio of amendment inputs
          amendment[,fr_dpm_rpm := fifelse(P_HC < 0.92, -2.174 * P_HC + 2.02, 0)]
          
          # Calculate average DPM-RPM ratio of amendment inputs
          DR_amendment <- amendment[P_DOSE > 0, weighted.mean(fr_dpm_rpm, w = (P_DOSE * P_C_OF), na.rm = TRUE)]
          
    }
    
  # Set average DPM-RPM ratio of amendment to 0 if not finite
    if (!is.finite(DR_amendment) || length(DR_amendment) == 0) DR_amendment <- 0
    
    
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

    # Define ratio CO2 / (BIO+HUM)
    x <- 1.67 * (1.85 + 1.60 * exp(-0.0786 * dt.soc$A_CLAY_MI))
    
    # transfer coefficients upon C degradation (B is BIO, H = HUM)
    B = 0.46/(x + 1)
    H = 0.54/(x + 1)
    
    # rates of the five pools, DPM, RPM, BIO, HUM and IOM 
    ks <- c(k1, k2, k3, k4, 0)
    
    # make C flow matrix based on k (removed) and transfer coefficients (added)
    A = diag(-ks)
    A[3, ] = A[3, ] + B * ks
    A[4, ] = A[4, ] + H * ks

    # Define average rate modifying factor over the crop rotation 
    xi <- mean(abcd(1:(12*isimyears)/12))
    
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
    
    # vector where crop inputs ends up: rho fraction DPM, (1-rho) fraction RPM, 0% BIO, 0% HUM
    rho_vector <- matrix(c(rho, 1 - rho, 0, 0), nrow = 4, ncol = 1)
    
    # vector where amendment ends up: tau fraction DPM, nu fraction RPM, 0% BIO, 2% HUM
    tau_nu_vector <- matrix(c(tau, nu, 0, 1 - tau - nu), nrow = 4, ncol = 1)
    
    # Combined input vector to know fraction of total C inputs ending up in different pools
    input_vector <- CR_proportion * rho_vector + M_proportion * tau_nu_vector
    
    ## calculate carbon inputs
    I4 <- matrix(c(1, 1, 1, 1), nrow = 1, ncol = 4) # row vector to sum across pools
    A4 <- A[1:4, 1:4] # C flow matrix without IOM flow

    # Check if C flow matrix is invertible
    if (abs(det(A4)) < 1e-10) {
      stop("Matrix A4 is singular - check decomposition rates in rothc.parms")
    }
    
    # Invert C flow matrix to calculate pool sizes at equilibrium
    A4inv <- tryCatch(
      solve(A4),
      error = function(e)stop("Failed to invert matrix A4: ", e$message)
    )
    
    # Calculate annual carbon input from amendments/crops assuming steady state (total C/residence time)
    I <- as.numeric(CTOT4 / (-(1 / xi) * I4 %*% A4inv %*% input_vector))
   
    # Check if I is finite and positive
    if (!is.finite(I) || I <= 0) {
      stop("spinup_Heuvelink initialisation produced invalid results. Check input data and parameters.")
      }
    
    # carbon pool calculation
    Q <- input_vector * I # Determine C input for each pool
    C <- -(1 / xi) * A4inv %*% Q # Establish C content for each pool at equilibrium
  
    # read out C pools under equilibrium state
    cpools <- list(CIOM0 = FallIOM,
                      CDPM0 = C[1],
                      CRPM0 = C[2],
                      CBIO0 = C[3])
  }
  
  ## calculate initial C pools assuming equilibrium in C stocks, using analytical solution (as implemented in BodemCoolstof tool)
  if(initialisation_method == 'spinup_analytical_bodemcoolstof'){

    # create local copy
    dt.soc <- copy(dt.soc)
    
    # recalculate total organic carbon from kg to ton C / ha
    dt.soc[,toc := toc/1000]
    
    # time correction (is 12 in documentation Chantals' study, not clear why, probably due to fixed time step calculation)
    timecor = 1

    # CDPM pool (ton C / ha)
    cdpm.ini <- rothc.event[var == 'CDPM',list(time,value)]
    if(nrow(cdpm.ini) >0){
    cdpm.ini[,cf_abcd := abcd(time)]
    cdpm.ini <- cdpm.ini[,((sum(value) * 0.001 / max(time)) / (mean(cf_abcd)/timecor))/k1]
    dt.soc[, cdpm.ini := mean(cdpm.ini)]
    }else{
      dt.soc[,cdpm.ini := 0]
    }
    
    # CRPM pool (ton C / ha)
    crpm.ini = rothc.event[var == 'CRPM',list(time,value)]
    if(nrow(crpm.ini) > 0) {
    crpm.ini[,cf_abcd := abcd(time)]
    crpm.ini <- crpm.ini[,((sum(value) * 0.001 / max(time)) / (mean(cf_abcd)/timecor))/k2]
    dt.soc[, crpm.ini := mean(crpm.ini)]
    }else{
      dt.soc[,crpm.ini := 0]
    }
    
    # CIOM pool (ton C / ha)
    dt.soc[, ciom.ini := 0.049 * toc^1.139]
    
    # CBIOHUM pool (ton C /ha)
    dt.soc[,biohum.ini := toc - ciom.ini - crpm.ini - cdpm.ini]
    
    # set to defaults (see RothC documentation) when calculated RPM + DPM pools exceed available carbon
    dt.soc[biohum.ini <0, cdpm.ini := 0.015 * (toc-ciom.ini)]
    dt.soc[biohum.ini <0, crpm.ini := 0.125 * (toc-ciom.ini)]
    dt.soc[, biohum.ini := toc-ciom.ini - crpm.ini - cdpm.ini]
    
    # CBIO and CHUM pool
    dt.soc[,cbio.ini := biohum.ini / (1 + k3 / k4)]
 
    # define fractions
    cpools <- dt.soc[,.(
      CIOM0 = ciom.ini,
      CDPM0 = cdpm.ini,
      CRPM0 = crpm.ini,
      CBIO0 = cbio.ini)]
    
  }
  
  # unlist fractions
  cpools <- unlist(cpools)
  
  # Return output
  return(cpools)
  
}