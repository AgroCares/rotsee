#' Function for simple RothC simulation to assess soil health for grassland and croplands
#'
#' This function calculates the optimal sequestration scenario for cropland and grassland with the RothC model over the course of 50 years
#' For cropland this function converts the crop to winter wheat where crop residue is left behind, grassland field are converted to permanent grassland
#' The function assumes that the P-status of the soil is neutral according to the Dutch fertilization regulation and applies the maximal permitted P-dose in the form of cattle slurry
#'
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param A_SOM_LOI (numeric) value for the soil organic matter content of the soil
#' @param A_CLAY_MI (numeric) value for the clay content of the soil
#' @param simyears (integer) value for the amount of years to simulate, default is 50 years
#' @param init (boolean) use internal analytical solution for initialisation RothC
#' @param scen (character) scenarios to be simulated. Options include BAU, BAUIMPR,CLT and ALL.
#' @param spinup (numeric) the spinup period that is used for initialisation model
#'
#' @import data.table
#'
#' @export
rc_shi_field <- function(B_LU_BRP, A_SOM_LOI, A_CLAY_MI, simyears = 50, init = FALSE,
                            scen = c('BAU','ALL'),spinup = 10){
  
  # check on inputs
  checkmate::assert_numeric(simyears,len=1,lower=15)
  checkmate::assert_character(scen)
  checkmate::assert_subset(scen,choices = c('BAU','BAUIMPR','CLT','ALL'))
  
  # a single field can only have one value for SOM and clay
  this.som <- mean(A_SOM_LOI)
  this.clay <- mean(A_CLAY_MI)
  this.soil <- list(A_SOM_LOI = this.som,
                    A_CLAY_MI = this.clay) 
  
  # initialize RothC with BAU for 150 years
  pool_fractions <- rc_initialise(soil_properties = this.soil)
  
  # run simulations for the desire scenarios
  sim <- list(); count <- 0
  
  # run the RothC model for two scenarios by default: BAU and ALL
  for(i in scen){
    
    # add counter
    count <- count + 1
    
    # prepare input for the scenarios
    scen.inp <- rc_input_scenario(B_LU_BRP = B_LU_BRP, scen = i)
    
    # retreive the rotation and amendment input data.tables
    rotation <- scen.inp$rotation
    amendment <- scen.inp$amendment
    
    # set seed
    set.seed(123)
    
    # Run simulation
    result <- rc_sim(soil_properties = this.soil,
                            A_DEPTH = 0.3,
                            rothc_rotation = rotation,
                            rothc_amendment = amendment,
                            rothc_parms = list(simyears = simyears + spinup,
                                               c_fractions = pool_fractions,
                                               initialize = init))
    # set startyear to zero
    result[,year := year - min(year)]
    
    # do a manual scaling for the spin-up period
    result[,A_SOM_LOI := A_SOM_LOI * this.som / A_SOM_LOI[spinup]]
    
    # remove the years before spinput
    result <- result[year >= spinup - 1]
    result[,year := year - min(year)]
    
    # save output in a list
    sim[[count]] <- data.table(result,scenario = i)
    
  }
  
  # combine scenarios
  dt.sim <- rbindlist(sim,fill=T)
  dt.sim <- dcast(dt.sim,year~scenario,value.var ='A_SOM_LOI')
  
  # update names
  setnames(dt.sim,old = scen,new = paste0('A_SOM_LOI_',scen),skip_absent=TRUE)
  
  # Return result
  return(dt.sim)
  
}