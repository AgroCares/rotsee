#' Multicore RothC simulation
#' 
#' Function to perform parallel RothC calculations via \link{rc_sim} using multicore processing.
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
#'
#' @import data.table
#' @import progressr
#' @import future.apply
#' @import future
#' @import parallelly
#'
#' @export
rc_multicore <- function(soil_properties,
                         rotation,
                         amendment,
                         A_DEPTH = 0.3,
                         B_DEPTH = 0.3,
                         parms,
                         weather,
                         final = FALSE,
                         quiet = TRUE){
  
  # add visual bindings
ID = NULL
  # Check if relevant packes are installed
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
checkmate::assert_set_equal(sort(unique(amendment$ID)), sort(unique(amendment$ID)))
checkmate::assert_data_table(weather)
  
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
                                           A_DEPTH = A_DEPTH,
                                           B_DEPTH = B_DEPTH,
                                           weather = weather,
                                           final = final,
                                           future.seed = TRUE,
                                           future.packages = c('rotsee'))
  })

  # close cluster
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


#' RothC parallel function
#' 
#' Function to run RothC parallel for a series of fields
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
 
    # run simulations for the desire scenarios
    sim <- list(); count <- 0

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
    if(final){out <- out[year > max(year) - 10,lapply(.SD,mean)]}

    # show progress
    if (! is.null(p)) {if (this.xs %% 10 == 0) p(sprintf('id = %g', this.xs))}
    
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
