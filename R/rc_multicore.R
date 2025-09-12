#' Multicore RothC simulation
#' 
#' Function to evaluate the carbon saturation via RothC simulation on multiple fields using multicore processing
#'
#' @param soil_properties (data.table)
#' @param rotation (data.table)
#' @param amendment (data.table)
#' @param A_DEPTH (numeric)
#' @param B_DEPTH (numeric)
#' @param parms (list)
#' @param weather (data.table)
#' @param quiet (boolean) showing progress bar for calculation RothC C-saturation for each field
#'
#' @import data.table
#' @import progressr
#' @import future.apply
#' @import future
#' @import parallelly
#'
#' @export
rc_multicore <- function(soil_properties,
                         rotation = NA_real_,
                         amendment = NA_real_,
                         A_DEPTH,
                         B_DEPTH,
                         parms = NA_real_,
                         weather = NA_real_,
                         quiet = TRUE){
  
  # add visual bindings

  # Check if relevant packes are installed
  if (system.file(package = 'future') == '') {stop('multicore processing requires future to be installed')}
  if (system.file(package = 'future.apply') == '') {stop('multicore processing requires the package future.apply to be installed')}
  if (system.file(package = 'parallelly') == '') {stop('multicore processing requires the package parallelly to be installed')}
  
  # Check inputs
# check each ID is supplied for each data point
  
# Ensure soil properties only has one value

  
  # Set RothC run parameters
  simulation_time <- 50L
  
  # multithreading
  cm.versions <- c('CM4')

  # add group
  rotation[,xs := .GRP,by = ID]
  amendment[,xs := .GRP,by = ID]
  soil_properties[,xs := .GRP,by = ID]

  # Run the simulations
  future::plan(future::multisession, workers = parallelly::availableCores()-1)

  # add group
  rotation[,xs := .GRP,by = ID]
  amendment[,xs := .GRP,by = ID]
  soil_properties[,xs := .GRP,by = ID]

  # run RothC function
  progressr::with_progress({
    xs <- sort(unique(rotation$xs))
    if(quiet){p = NULL} else {p <- progressr::progressor(along = xs)}
    
    results <- future.apply::future_lapply(X = xs,
                                           FUN = rotsee::rc_parallel,
                                           soil_properties = soil_properties,
                                           rotation = rotation,
                                           amendment = amendment,
                                           parms = parms,
                                           p = p,
                                           weather = weather,
                                           future.seed = TRUE,
                                           future.packages = c('rotsee'))
  })

  # close cluster
  future::plan(future::sequential)
  
  # combine output
  dt.res <- rbindlist(results, fill = TRUE)

  # columns to select
 # mcols <- paste0('A_SOM_LOI_',scen)
  
  # merge with dt.c
  dt.c <- merge(dt.c,
                dt.res[,mget(c('xs',mcols))],
                by='xs',all.x=TRUE)
  
  # retreive unique value per ID
  dt.c <- dt.c[,lapply(.SD,mean),.SDcols = mcols,by=ID]
  
  # return
  return(dt.c)
}


#' RothC parallel function
#' 
#' Function to run RothC parallel for a series of fields
#' 
#' @param this.xs (numeric) selected id for a single field
#' @param soil_properties (data.table)
#' @param rotation (data.table)
#' @param amendment (data.table)
#' @param A_DEPTH (numeric)
#' @param B_DEPTH (numeric)
#' @param parms (list)
#' @param weather (data.table)
#' @param p (progress bar) progress bar
#' @param final (boolean) option to select only the last year
#'
#' @export
rc_parallel <- function(this.xs,
                        soil_properties,
                        rotation = NA_real_,
                        amendment = NA_real_,
                        A_DEPTH = 0.3,
                        B_DEPTH = 0.3,
                        parms = NA_real_,
                        weather = NA_real_,
                        p,
                        final = FALSE){
  
  
  # set visual binding
 
  
  # get simulation data
   
  this.rotation <- rotation[xs == this.xs]
  this.amendment <- amendment[xs == this.xs]
  this.soil <- soil_properties[xs == this.xs]
  
  # set seed
  mc <- 111
  # do RothC simulation
  result <- tryCatch({
    
    # set seed
    set.seed(mc)
 
    # run simulations for the desire scenarios
    sim <- list(); count <- 0

    # Run the RothC model
    out <- rotsee::rc_sim(
                  soil_properties = this.soil,
                  A_DEPTH = 0.3,
                  B_DEPTH = 0.3,
                  cf_yield = 1,
                  M_TILLAGE_SYSTEM = "CT",
                  rothc_rotation = this.rotation,
                  rothc_amendment = this.amendment,
                  rothc_parms = parms)
    
    out[,xs := this.xs]

    # if final is true, calculate mean of last 10 years
    if(final){out <- out[year > max(year) - 10,lapply(.SD,mean)]}

    # show progress
    if (! is.null(p)) {if (this.xs %% 10 == 0) p(sprintf('id = %g', this.xs))}
    
    result <- copy(out)
 
    return(result)
   
  }, error = function (e) {
    
    
    if(final){
      result <- data.table(year = year(parms$end_date), A_SOM_LOI_BAU = 0, A_SOM_LOI_ALL = 0,xs = this.xs)
    } else{
      result <- data.table(A_SOM_LOI_BAU = 0, A_SOM_LOI_ALL = 0,xs = this.xs)
    }
    result$error <- as.character(e)
    
    if (! is.null(p)) {p(sprintf('id %g has error: %s', this.xs, as.character(e)))}
    
    return(result)
  })

  
  return(result)
  
}
