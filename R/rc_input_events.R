#' Combine all EVENT data needed for RothC modelling
#'
#' This function combines required inputs into a data.table that is needed as input for the RothC model.
#'
#' @param crops (data.table) Table containing carbon inputs of DPM and RPM from crops, calculated in rc_input_event_crop
#' @param amendment (data.table) Table containing carbon inputs of DPM, RPM, and HUM from amendments, calculated in rc_input_event_amendment
#' @param simyears (numeric) Amount of years for which the simulation should run, Default: 50 years
#'
#' @export
rc_input_events <- function(crops,amendment,dt.time){
  
  # add visual bindings
  id = time = yr_rep = NULL
  
  # check parameters
  checkmate::assert_data_table(crops, any.missing = FALSE)
  checkmate::assert_names(colnames(crops), must.include = c("time", "var", "value", "method"))
  checkmate::assert_data_table(amendment, any.missing = FALSE)
  checkmate::assert_names(colnames(amendment), must.include = c("time", "var", "value", "method"))
  # create event
  rothc.event <- rbind(crops,amendment)
  
  # Return file if rothc event is empty
  if(nrow(rothc.event) == 0L){
    return(rothc.event)
  }

  # align time for amendment and crop events
  rothc.event[, time := time - floor(min(time))]

  # sum multiple additives that are given at same time
  rothc.event <- rothc.event[,list(value = sum(value)),by = c( 'time','var','method')]

  # order
  setorder(rothc.event,time)
 
  # return output
  return(rothc.event)
}

#' Calculate the crop rotation related C inputs of a field on monthly basis
#'
#' This function determines how much Carbon enters the soil throughout the year given the crop rotation plan.
#'
#' @param crops (data.table) Table with crop rotation, crop management measures, year and potential Carbon inputs.
#' @param dt.time (data.table) Table containing all combinations of months and years in the simulation period
#'
#' @export
rc_input_event_crop <- function(crops,dt.time){
  
  # add visual bindings
  crop_name = B_LU = NULL
  M_GREEN_TIMING = M_CROPRESIDUE = green_eom = NULL
  crflt = cin_dpm = cin_crop_dpm = cin_res_dpm = cin_rpm = cin_crop_rpm = cin_res_rpm = NULL
  cin_crop = tcf = method = cf_yield = crop_code = time = NULL
  
  # check inputs
  arg.length <- nrow(crops)
  
  # check crops input data.table
  checkmate::assert_data_table(crops, nrows = arg.length)
  checkmate::assert_true(all(c('year','cin_dpm','cin_rpm') %in% names(crops)))
  checkmate::assert_integerish(crops$year, any.missing = FALSE, len = arg.length)
  if (!"month" %in% names(crops)) crops[, month := NA_real_]
  
  
  
  # make internal copy
  dt <- copy(crops)

  # setorder
  setorder(dt,year,month)

  # add cumulative time vector
  dt <- merge(dt.time, dt, by = c('year','month'), all.x = T)
  
  # select only relevant columns as output for EVENT crop residue input
  # and select only those time steps where C input is bigger than zero
  out1 <- dt[cin_dpm > 0 | cin_rpm > 0,list(CDPM = cin_dpm,CRPM = cin_rpm,time = time)]
  
  # melt the output table
  out1 <- melt(out1,id.vars = "time", variable.name = "var")
  
  # add method how RothC should treat the event
  out1[, method := 'add']

  # return output
  return(out1)
}

#' Calculate the monthly timing of carbon inputs for different fertilizer strategies
#'
#' This function calculates the timing of carbon inputs (kg C per ha) based on type of organic matter amendments and land use.
#'
#' @param crops (data.table) Table with crop rotation, year and potential Carbon inputs.
#' @param amendment (data.table) A table with the following column names: P_ID, P_NAME, year, month, cin_tot, cin_hum, cin_dpm, and cin_rpm.
#' @param dt.time (data.table) Table containing all combinations of months and years in the simulation period
#'
#' @details This function increases temporal detail for time series of C inputs of organic amendments.
#' The inputs for organic amendments are organised in the data.table amendment, where the carbon inputs has the unit kg C / ha.
#'
#' The output is an EVENT object.
#'
#' @export
rc_input_event_amendment <- function(crops,amendment = NULL, dt.time){
  
  # add visual bindings
  B_LU = B_LU_NAME = p_cat = fre_eoc_p = crflt = tcf = NULL
  cin_hum = cin_rpm = cin_dpm = method = crop_code = crop_name = NULL
  time = NULL
  
  # make local copy
  dt <- copy(amendment)
  
  # make default crop amendment data.table when dt = NULL
  if(is.null(dt)){dt <- data.table(year = crops[1,year], month = 1, cin_tot = 0, cin_hum = 0,
                                   cin_dpm = 0, cin_rpm = 0)}

  # do checks on the crop list
  checkmate::assert_data_table(crops)
  checkmate::assert_true('B_LU' %in% colnames(crops))
  checkmate::assert_true('year' %in% colnames(crops))
  
  # do checks on the input of C due to organic amendments
  checkmate::assert_data_table(dt)
  
  allowed <- c('year','month','p_ID','p_name','cin_tot','cin_hum',
               'cin_dpm','cin_rpm')
  checkmate::assert_subset(colnames(dt), allowed, empty.ok = FALSE)
  
  checkmate::assert_numeric(dt$cin_hum,lower = 0, upper = 100000,len = nrow(dt))
  checkmate::assert_numeric(dt$cin_tot,lower = 0, upper = 100000,len = nrow(dt))
  checkmate::assert_numeric(dt$cin_dpm,lower = 0, upper = 100000,len = nrow(dt))
  checkmate::assert_numeric(dt$cin_rpm,lower = 0, upper = 100000,len = nrow(dt))
  checkmate::assert_integerish(dt$year,len = nrow(dt))
  
  # add cumulative time vector
  dt <- merge(dt.time, dt, by = c('year','month'), all.x = T)
  
  # select only those events that manure input occurs
  dt <- dt[cin_hum > 0 | cin_rpm > 0 | cin_dpm > 0]

  # select only relevant columns, rename them
  out <- dt[,list(CDPM = cin_dpm,CRPM = cin_rpm,CHUM = cin_hum,time = time)]
 
  # melt the output table
  out <- melt(out,id.vars = "time", variable.name = "var")

  # add method how RothC should treat the event
  out[, method := 'add']
  
  # return output
  return(out)
}
