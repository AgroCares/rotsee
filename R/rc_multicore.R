#' RothC parallel function
#' 
#' Function to run RothC parallel for a series of fields
#' 
#' 
#' @param this.xs (numeric) selected id for a single field
#' @param dt.c (data.table) set will all fields
#' @param scen (character) scenarios to be simulated. Options include BAU, BAUIMPR, CLT and ALL.
#' @param simyears (integer) value for the amount of years to simulate, default is 50 years
#' @param p (progress bar) progress bar
#' @param final (boolean) option to select only the last year
#'
#' @export
rc_parallel <- function(this.xs, dt.c, scen, simyears = 50,p = NULL,final = TRUE){
  
  # set visual binding
  xs = NULL
  
  # get simulation data
  sim.dt <- dt.c[xs == this.xs]
  
  mc <- sim.dt$mc[1]
  
  # do RothC simulation
  result <- tryCatch({
    
    # set seed
    set.seed(mc)
    
    # take average SOM and clay per field
    this.som <- mean(sim.dt$A_SOM_LOI)
    this.clay <- mean(sim.dt$A_CLAY_MI)
    
    # run RothC
    out <- rc_field(B_LU_BRP = sim.dt$B_LU_BRP,
                           A_SOM_LOI = this.som,
                           A_CLAY_MI =  this.clay,
                           simyears = simyears,
                           init = FALSE,
                           scen = scen,
                           spinup = 10)
    out[,xs := this.xs]
    
    # if final is true select only last prediction
    if(final){out <- out[year > max(year) - 10,lapply(.SD,mean)]}
    
    # show progress
    if (! is.null(p)) {if (this.xs %% 10 == 0) p(sprintf('id = %g', this.xs))}
    
    result <- copy(out)
    
    return(result)
    
  }, error = function (e) {
    
    
    if(final){
      result <-data.table(year = simyears,A_SOM_LOI_BAU = 0, A_SOM_LOI_ALL = 0,xs = this.xs)
    } else{
      result <- data.table(year = 1:simyears,A_SOM_LOI_BAU = 0, A_SOM_LOI_ALL = 0,xs = this.xs)
    }
    result$error <- as.character(e)
    
    if (! is.null(p)) {p(sprintf('id %g has error: %s', this.xs, as.character(e)))}
    
    return(result)
  })
  
  
  return(result)
  
}


#' Multicore RothC simulation
#' 
#' Function to evaluate the carbon saturation via RothC simulation on multiple fields using multicore processing
#'
#' @param ID (character) A field id
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param B_GWL_GLG (numeric) The lowest groundwater level averaged over the most dry periods in 8 years in cm below ground level
#' @param A_SOM_LOI (numeric) value for the soil organic matter content of the soil
#' @param A_CLAY_MI (numeric) value for the clay content of the soil
#' @param scen (character) scenarios to be simulated. Options include BAU, BAUIMPR,CLT and ALL.
#' @param simyears (integer) value for the amount of years to simulate, default is 50 years
#' @param quiet (boolean) showing progress bar for calculation RothC C-saturation for each field
#'
#' @import data.table
#' @import progressr
#' @import future.apply
#' @import future
#' @import parallelly
#'
#' @export
rc_multicore <- function(ID,B_LU_BRP,B_GWL_GLG,A_SOM_LOI,A_CLAY_MI, scen, simyears = 50, quiet = TRUE){
  
  # add visual bindings
  A_SOM_LOI_ALL = A_SOM_LOI_BAU = . = NULL
  
  # Check if relevant packes are installed
  if (system.file(package = 'future') == '') {stop('multicore processing requires future to be installed')}
  if (system.file(package = 'future.apply') == '') {stop('multicore processing requires the package future.apply to be installed')}
  if (system.file(package = 'parallelly') == '') {stop('multicore processing requires the package parallelly to be installed')}
  
  # Check inputs
  arg.length <- max(length(B_LU_BRP), length(A_SOM_LOI), length(A_CLAY_MI))
  checkmate::assert_numeric(B_LU_BRP, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_subset(B_LU_BRP, choices = unique(BLN::bln_crops$crop_code), empty.ok = FALSE)
  checkmate::assert_numeric(A_CLAY_MI, lower = 0, upper = 100, any.missing = FALSE, min.len = 1)
  checkmate::assert_numeric(A_SOM_LOI, lower = 0.1, upper = 100, any.missing = FALSE, min.len = 1, len = arg.length)
  checkmate::assert_numeric(B_GWL_GLG, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(scen)
  checkmate::assert_subset(scen,choices = c('BAU','BAUIMPR','CLT','ALL'))
  checkmate::assert_numeric(simyears,len=1,lower=15)
  
  if(length(ID)>1){checkmate::assert_true(length(ID) == arg.length)}
  checkmate::assert_logical(quiet)
  
  #make internal table (set clay to 75 max given checkmate)
  dt.c <- data.table(ID = ID,
                     B_LU_BRP = B_LU_BRP,
                     B_GWL_GLG = B_GWL_GLG,
                     A_SOM_LOI = A_SOM_LOI,
                     A_CLAY_MI= pmin(75,A_CLAY_MI))
  
  # RothC
  simulation_time <- 50L
  
  # multithreading
  cm.versions <- c('CM4')
  
  # Run the simulations
  future::plan(future::multisession, workers = parallelly::availableCores()-1)
  
  # add seed
  dt.c$mc <- 111
  
  # add group
  dt.c[,xs := .GRP,by = ID]
  
  # run RothC function
  progressr::with_progress({
    xs <- sort(unique(dt.c$xs))
    if(quiet){p = NULL} else {p <- progressr::progressor(along = xs)}
    
    results <- future.apply::future_lapply(X = xs,
                                           FUN = rotsee::rc_parallel,
                                           dt.c = dt.c,
                                           p = p,
                                           scen = scen,
                                           simyears = simyears,
                                           future.seed = TRUE,
                                           future.packages = c('rotsee'))
  })
  
  # close cluster
  future::plan(future::sequential)
  
  # combine output
  dt.res <- rbindlist(results, fill = TRUE)
  
  # columns to select
  mcols <- paste0('A_SOM_LOI_',scen)
  
  # merge with dt.c
  dt.c <- merge(dt.c,
                dt.res[,mget(c('xs',mcols))],
                by='xs',all.x=TRUE)
  
  # retreive unique value per ID
  dt.c <- dt.c[,lapply(.SD,mean),.SDcols = mcols,by=ID]
  
  # return
  return(dt.c)
}

