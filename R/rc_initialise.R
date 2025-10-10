#' Function for initializing the RothC model
#'
#' @param crops (data.table) Table with crop rotation, cultivation management, year and potential Carbon inputs.
#' @param amendment (data.table) A table with the following column names: year, month, cin_tot, cin_hum, cin_dpm, cin_rpm and the fraction eoc over p (fr_eoc_p). Month is optional.
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param dt.soc (data.table) Data table containing information on soil properties. See details for information.
#' @param dt.time (data.table) Combination of months and years of the entire simulation period. 
#' @param rothc.parms (list) List with relevant RothC parameters. See details for more information
#' @param rothc.event (data.table) List with events of C inputs. See details for more information
#' @param type (character) options for spin-up (spinup_simulation,spinup_analytical_bodemcoolstof, spinup_analytical_heuvelink)
#'
#' @import data.table
#' @importFrom stats weighted.mean
#' 
#' @details
#' The crop table used as input for carbon modelling requires at minimum data on effective organic matter inputs and related year.
#' To run this function, the dt requires as input: B_LU (a crop id), B_LU_NAME (a crop name, optional), B_LU_EOM (the effective organic matter content, kg/ha), B_LU_EOM_RESIDUE (the effective organic matter content for crop residues, kg/ha), and the B_LU_HC (the humification coeffient,-).
#' if crops is NULL, then the crop input will be prepared using function \link{rc_input_scenario} using scenario 'BAU'
#' The same is done for the amendment data.table. This table requires as input:"P_NAME", "year","month","P_OM","P_HC","p_p2o5", and "P_DOSE"
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
#' * A_C_OF (numeric), soil organic carbon content (g C/kg), preferably for soil depth 0.3 m
#' * B_C_ST03 (numeric), soil organic carbon stock (Mg C/ha), preferably for soil depth 0.3 m. Required if A_C_OF is not supplied
#' * A_CLAY_MI (numeric), clay content (\%)
#' * toc
#' * A_DENSITY_SA (numeric) bulk density of the soill (g/cm3), can be calculated using \link{rc_calculate_bd}
#'
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
                          dt.time,
                          type ='spinup_analytical_bodemcoolstof'){
  
  # add visual bindings
  . = CIOM = CDPM = CRPM = CBIO = B_LU_EOM = M_CROPRESIDUE = B_LU_HC = chum.ini = NULL
  P_DOSE = P_OM = M_GREEN_TIMING = fr_dpm_rpm = P_HC = B_LU_EOM_RESIDUE = NULL
  abc = bd = time = toc = var = cf_abc = ciom.ini = biohum.ini = cbio.ini = NULL

  # Define rothc parameters
  k1 <- rothc.parms$k1
  k2 <- rothc.parms$k2
  k3 <- rothc.parms$k3
  k4 <- rothc.parms$k4
  
  abc <- rothc.parms$abc
  # initialise options
  
  # do a simulation for 150 years to estimate the C fractions assuming system is in equilibrium
  if(type =='spinup_simulation'){
    
    # Set model parameters
    parms <- list(simyears = 150,unit = 'psomperfraction', initialize = FALSE)
    
    # Set newly required inputs
    
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
    
  }
  
  # calculate initial carbon pools assuming equilibrium in C pools, using analytical solution (Heuvelink)
  if(type=='spinup_analytical_heuvelink'){

    # averaged total C input (kg C/ha/year) from crop, crop residues, catch crops and amendments
    c_input_crop <- crops[,sum(B_C_OF_INPUT)/max(dt.time$time)]
    c_input_man <- if(!is.null(amendment$B_C_OF_INPUT) && all(!is.na(amendment$B_C_OF_INPUT))){
      amendment[, sum(B_C_OF_INPUT)/max(dt.time$time)]
    }else{
      amendment[,sum(P_DOSE * P_C_OF)/max(dt.time$time)]
    }

    # calculate C input ratio of crop and amendments
    CR_proportion <- (c_input_crop) / (c_input_crop+c_input_man)
    M_proportion <- c_input_man / (c_input_crop+c_input_man)
    
    # estimate DPM-RPM ratio of the inputs from crop and amendments
    crops[,fr_dpm_rpm := fifelse(B_LU_HC < 0.92, -2.174 * B_LU_HC + 2.02, 0)]
    amendment[,fr_dpm_rpm := fifelse(P_HC < 0.92, -2.174 * P_HC + 2.02, 0)]
    
    # calculate average dpm_rpm ratio of C inputs from crop and amendments
    DR_crop <- crops[,weighted.mean(fr_dpm_rpm,w=(B_C_OF_INPUT))]
    if(!is.null(amendment$B_C_OF_INPUT) && all(!is.na(amendment$B_C_OF_INPUT))){
      DR_amendment <- amendment[P_DOSE >0,weighted.mean(fr_dpm_rpm,w=(B_C_OF_INPUT))]
    }else{
    DR_amendment <- amendment[P_DOSE >0,weighted.mean(fr_dpm_rpm,w=(P_DOSE * P_C_OF))]
    }
    
    # what is length of the crop rotation in years
    isimyears <- max(dt.time$year)
    
    # ratio CO2 / (BIO+HUM)
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
 
    # averaged rate modifying factor over the crop rotation 
    xi <- mean(abc(1:(12*isimyears)/12))
    
    # rho is fraction plant material as DPM
    rho <- DR_crop / (1 + DR_crop)
    
    # fraction of DPM (tau), RPM (nu), and 2% HUM in FYM
    tau <- (1 - 0.02) * DR_amendment / (1 + DR_amendment)
    nu <- (1 - 0.02) * 1 / (1 + DR_amendment)
    
    # total SOC stock (ton C / ha) from bulk density 
    CTOT <- dt.soc$toc * 1000
      
    # IOM pool (ton C / ha) using Falloon method
    FallIOM <- 0.049 * CTOT^1.139
    
    # Active SOC pool (ton C / ha)
    CTOT4 <- CTOT - FallIOM
    
    # vector where crop inputs ends up: rho % DPM, (1-rho) % RPM, 0% BIO, 0% HUM
    rho_vector <- matrix(c(rho, 1 - rho, 0, 0), nrow = 4, ncol = 1)
    
    # vector where amendment ends up: tau % DPM, nu % RPM, 0% BIO, 2% HUM
    tau_nu_vector <- matrix(c(tau, nu, 0, 1 - tau - nu), nrow = 4, ncol = 1)
    
    # Combined input vector to know fraction of total C inputs ending up in pools
    input_vector <- CR_proportion * rho_vector + M_proportion * tau_nu_vector
    
    # calculate carbon input
    I4 <- matrix(c(1, 1, 1, 1), nrow = 1, ncol = 4)
    A4 <- A[1:4, 1:4]
    A4inv <- solve(A4)
    I <- as.numeric(CTOT4 / (-(1 / xi) * I4 %*% A4inv %*% input_vector))
    
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
  
  # calculate initial C pools assuming equilibrium in C stocks, using analytical solution (as implemented in BodemCoolstof tool)
  if(type=='spinup_analytical_bodemcoolstof'){

    # recalculate total organic carbon from kg to ton C / ha
    dt.soc[,toc := toc/1000]
    
    # time correction (is 12 in documentation Chantals' study, not clear why, probably due to fixed time step calculation)
    timecor = 1

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
 
    # define fractions
    fractions <- dt.soc[,.(fr_IOM = ciom.ini / toc,
                           fr_DPM = cdpm.ini / toc,
                           fr_RPM = crpm.ini / toc,
                           fr_BIO = cbio.ini / toc)]
    
    }
  
  # unlist fractions
  fractions <- unlist(fractions)
  
  # Return output
  return(fractions)
  
}