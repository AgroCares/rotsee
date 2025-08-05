#' Function for initializing the RothC model
#'
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param A_SOM_LOI (numeric) value for the soil organic matter content of the soil
#' @param A_CLAY_MI (numeric) value for the clay content of the soil
#'
#' @import data.table
#'
#' @export
rc_initialise <- function(B_LU_BRP,A_SOM_LOI,A_CLAY_MI,type ='A'){
  
  # add visual bindings
  . = CIOM = CDPM = CRPM = CBIO = NULL
  
  # Prepare input for scenario Business As Usual
  scen.inp <- rc_input_scenario(B_LU_BRP = B_LU_BRP, scen = 'BAU')
  rotation <- scen.inp$rotation
  amendment <- scen.inp$amendment
  
  # initialise options
  
  # do a simulation for 150 years to estimate the C fractions assuming system is in equilibrium
  if(type =='spinup_simulation'){
    
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
    
  }
  
  # calulate initial carbon pools at equilibrium using analytical solution (Heuvelink)
  if(type=='spinup_analytical_heuvelink'){
    
    # averaged total C input (kg C/ha/year)
    c_input_crop <- rotation[,sum(B_LU_EOM * 0.5 / B_LU_HC + B_LU_EOM* 0.5 / B_LU_HC)/max(year)]
    c_input_man <- amendment[,sum(P_DOSE * P_OM * 0.01 * 0.5)/max(year)]
    
    # calculate C input ratio of crop and amendments
    CR_proportion <- c_input_crop / (c_input_crop+c_input_man)
    M_proportion <- c_input_man / (c_input_crop+c_input_man)
    
    # estimate DPM-RPM ratio of the inputs from crop and amendments
    rotation[,fr_dpm_rpm := fifelse(B_LU_HC < 0.92, -2.174 * B_LU_HC + 2.02, 0)]
    amendment[,fr_dpm_rpm := fifelse(P_HC < 0.92, -2.174 * P_HC + 2.02, 0)]
    
    # calculate average dpm_rpm ratio of C inputs from crop and amendments
    DR_crop <- rotation[,weighted.mean(fr_dpm_rpm,w=(B_LU_EOM+B_LU_EOM_RESIDUE))]
    DR_amendment <- amendment[P_DOSE >0,weighted.mean(fr_dpm_rpm,w=(P_DOSE * P_OM))]
      
    # what is length of the crop rotation in years
    isimyears <- max(rotation$year)
    
    # make rate modifying factors input function
    dt.rmf <- rc_input_rmf(dt = rotation,B_LU_BRP = NULL,A_CLAY_MI = A_CLAY_MI, 
                           B_DEPTH = 0.3,simyears = isimyears, cf_yield = 1)
    
    # ratio CO2 / (BIO+HUM)
    x <- 1.67 * (1.85 + 1.60 * exp(-0.0786 * clay))
    
    # transfer coefficients (B is BIO, H = HUM)
    B = 0.46/(x + 1)
    H = 0.54/(x + 1)
    
    # rates of the five pools, DPM, RPM, BIO, HUM and IOM
    ks <- c(10, 0.3, 0.66, 0.02, 0)
    
    # make C flow matrix
    A = diag(-ks)
    A[3, ] = A[3, ] + B * ks
    A[4, ] = A[4, ] + H * ks
    
    # averaged rate modifying factor over the crop rotation 
    xi <- mean(dt.rmf$abc(1:(12*isimyears)/12))
    
    # rho is fraction plant material as DPM
    rho <- DR_crop / (1 + DR_crop)
    
    # fraction of FYM, 49% DPM (tau) and 49% RPM (nu), and 2% HUM as being used in RothC
    # adjust this to our implementation where FYM fractions also depend on DPM-RPM ratio
    #tau <- 0.49 ; nu <- 0.49
    tau <- (1 - 0.02) * DR_amendment / (1 + DR_amendment)
    nu <- (1 - 0.02) * 1 / (1 + DR_amendment)

    # total SOC stock (ton C / ha) from bulk density (estimated with Dutch pedotransferfunction)
    bd <- (1 / (0.02525 * A_SOM_LOI + 0.6541)) 
    CTOT <- A_SOM_LOI * 100 * 100 * 0.3 * bd * 0.01 * 0.5 
      
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
  
  # unlist fractions
  fractions <- unlist(fractions)
  
  # Return output
  return(fractions)
  
}