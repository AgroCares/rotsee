#' Parallel RothC calculations
#' 
#' Function to perform parallel RothC calculations via \link{rc_sim} using multicore processing. Required inputs and parameters should be identical to \link{rc_sim}
#'
#' @param soil_properties (data.table) Data table with soil properties. For inputs, see \link{rc_sim} with additional ID column used to identify scenario.
#' @param rotation (data.table) Table with crop rotation details and crop management actions that have been taken. For inputs, see \link{rc_sim} with additional ID column used to identify scenario.
#' @param amendment (data.table) Table with amendment input details. For inputs, see \link{rc_sim} with additional ID column used to identify scenario.
#' @param A_DEPTH (numeric) Depth for which soil sample is taken (m). Default set to 0.3.
#' @param B_DEPTH (numeric) Depth of the cultivated soil layer (m), simulation depth. Default set to 0.3.
#' @param parms (list) A list with simulation parameters controlling the dynamics of RothC Model. For inputs, see \link{rc_sim}.
#' @param weather (data.table) Table with weather information. For inputs, see \link{rc_sim}. 
#' @param quiet (boolean) showing progress bar for calculation RothC C-saturation for each field
#' @param final (boolean) option to select only the average of the last 10 years
#' @param strategy (character) select strategy for multistep calculations. Options are 'sequential' (single step calculation), 'multicore' (linux) and 'multisession' (windows)
#' @param cores (numeric) number of cores that can be used for multi-step calculations. 
#'
#' @import data.table
#' @import progressr
#' @import future.apply
#' @import future
#' @import parallelly
#'
#' @export
rc_sim_multi <- function(soil_properties,
                         rotation,
                         amendment,
                         A_DEPTH = 0.3,
                         B_DEPTH = 0.3,
                         parms,
                         weather,
                         final = FALSE,
                         quiet = TRUE,
                         strategy = "sequential",
                         cores = NULL){
  
  # add visual bindings
ID = . = xs = NULL
  # Check if relevant packages are installed
  if (system.file(package = 'future') == '') {stop('multicore processing requires future to be installed')}
  if (system.file(package = 'future.apply') == '') {stop('multicore processing requires the package future.apply to be installed')}
  if (system.file(package = 'parallelly') == '') {stop('multicore processing requires the package parallelly to be installed')}

  # Check inputs (for input value checks, see rc_sim)
checkmate::assert_data_table(soil_properties)
checkmate::assert_true(nrow(soil_properties) >= 1)
checkmate::assert_names(colnames(soil_properties), must.include = "ID")
checkmate::assert_true(!anyDuplicated(soil_properties$ID)) # Allow only one row per ID
checkmate::assert_data_table(rotation)
checkmate::assert_names(colnames(rotation), must.include = "ID")
checkmate::assert_data_table(amendment)
checkmate::assert_names(colnames(amendment), must.include = "ID")
checkmate::assert_set_equal(sort(unique(rotation$ID)), sort(unique(soil_properties$ID)))
checkmate::assert_set_equal(sort(unique(amendment$ID)), sort(unique(soil_properties$ID)))
checkmate::assert_data_table(weather)
checkmate::assert_character(strategy)
checkmate::assert_choice(strategy, c("sequential", "multisession", "multicore"))
if(!is.null(cores)) {
  checkmate::assert_integerish(cores, lower = 1)
  checkmate::assert(cores <= parallelly::availableCores(),
                          msg = 'requested cores exceed available cores')
}

  # add group (xs)
  soil_properties[,xs := .GRP,by = ID]
  rotation <- merge(rotation, soil_properties[, .(ID, xs)], by = "ID", all.x = TRUE)
  amendment <- merge(amendment, soil_properties[, .(ID, xs)], by = "ID", all.x = TRUE)


  # Run the simulations
 
  # Define number of cores used
  if(!is.null(cores)){
    workers <- max(1L, cores)
  }else{
  # Base used cores on those available
  workers <- max(1L, parallelly::availableCores() - 1L) # Define workers
  }
  
  # Define plan based on supplied strategy
  if(strategy == 'multisession') future::plan(future::multisession, workers = workers)
  if(strategy == 'multicore') future::plan(future::multicore(), workers = workers)
  if(strategy == 'sequential') future::plan(future::sequential())
  
  # run RothC function
  progressr::with_progress({
    xs <- sort(unique(soil_properties$xs))
    if(quiet){p <- NULL} else {p <- progressr::progressor(along = xs)}
    
    results <- future.apply::future_lapply(X = xs,
                                           FUN = rc_sim_multistep,
                                           soil_properties = soil_properties,
                                           rotation = rotation,
                                           amendment = amendment,
                                           parms = parms,
                                           p = p,
                                           A_DEPTH = A_DEPTH,
                                           B_DEPTH = B_DEPTH,
                                           weather = weather,
                                           final = final,
                                           future.seed = TRUE,
                                           future.packages = c('rotsee'))
  })

  # Return plan to sequential
  future::plan(future::sequential)
  
  # combine outputs
  dt.res <- rbindlist(results, fill = TRUE)

  # add original ID to identify treatment type 
  dt.res <- merge(dt.res,
                  soil_properties[,mget(c('ID', 'xs'))],
                  by = 'xs', all.x = TRUE)

  # return results
  return(dt.res)
}


#' RothC multistep function
#' 
#' Intermediary function called by \link{rc_sim_multi} to perform parallel rothc calculations
#' 
#' @param this.xs (numeric) selected id for a single field
#' @param soil_properties (data.table) Data table with soil properties. For inputs, see \link{rc_sim} with additional column 'xs' as identifier for separate scenario's
#' @param rotation (data.table) Table with crop rotation details and crop management actions that have been taken. For inputs, see \link{rc_sim} with additional column 'xs' as identifier for separate scenario's
#' @param amendment (data.table) Table with amendment input details. For inputs, see \link{rc_sim}
#' @param A_DEPTH (numeric) Depth for which soil sample is taken (m). Default set to 0.3.
#' @param B_DEPTH (numeric) Depth of the cultivated soil layer (m), simulation depth. Default set to 0.3.
#' @param parms (list) A list with simulation parameters controlling the dynamics of RothC Model. For inputs, see \link{rc_sim}.
#' @param weather (data.table) Table with weather information. For inputs, see \link{rc_sim}.
#' @param p (progress bar) progress bar
#' @param final (boolean) option to select only the average of the last 10 years

rc_sim_multistep <- function(this.xs,
                        soil_properties,
                        rotation = NULL,
                        amendment = NULL,
                        A_DEPTH = 0.3,
                        B_DEPTH = 0.3,
                        parms = NULL,
                        weather = NULL,
                        p,
                        final = FALSE){
  
  
  # set visual binding
 xs = NULL
  
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

    # Run the RothC model
    out <- rotsee::rc_sim(
                  soil_properties = this.soil,
                  A_DEPTH = A_DEPTH,
                  B_DEPTH = B_DEPTH,
                  rothc_rotation = this.rotation,
                  rothc_amendment = this.amendment,
                  weather = weather,
                  rothc_parms = parms)
    
    out[,xs := this.xs]

    # if final is true, calculate mean of last 10 years
    if (final) {
      num_cols <- names(out)[vapply(out, is.numeric, logical(1))] # select all numeric columns
      out <- out[year > max(year) - 10, lapply(.SD, mean), .SDcols = num_cols][, xs := this.xs]
      }

    # show progress
    if (!is.null(p)) p(sprintf('id = %g', this.xs))
    
    result <- copy(out)

    return(result)
   
  }, error = function (e) {
    
  
    if(final){
      result <- data.table(year = year(parms$end_date), A_SOM_LOI = 0, soc = 0,xs = this.xs)
    } else{
      result <- data.table(A_SOM_LOI = 0, soc = 0,xs = this.xs)
    }
    result$error <- as.character(e)
    
    if (! is.null(p)) {p(sprintf('id %g has error: %s', this.xs, as.character(e)))}
   
    return(result)
  })

  
  return(result)
  
}
