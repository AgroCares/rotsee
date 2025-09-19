#' Combine all EVENT data needed for RothC modelling
#'
#' This function combines required inputs into a data.table that is needed as input for the RothC model.
#'
#' @param crops (data.table) Table with crop rotation, cultivation management, year and potential Carbon inputs.
#' @param amendment (data.table) year, month (optional, defaults to 9), cin_hum, cin_dpm, and cin_rpm, cin_tot (optional). 
#' @param simyears (numeric) Amount of years for which the simulation should run, default: 50 years
#'
#' @export
rc_input_events <- function(crops = NULL,amendment = NULL, simyears = 50L){
  
  # add visual bindings
  id = time = yr_rep = NULL
  
  # Add checks on simyears
  checkmate::assert_integerish(simyears, lower = 1L)
  
  # estimate carbon inputs from the crop rotation plan
  event.crop <- rc_input_event_crop(crops)
  
  # estimate carbon inputs from amendment additions
  event.man <- rc_input_event_amendment(amendment)
  
  # create event
  rothc.event <- rbind(event.crop,event.man)
  
  # Return file if rothc event is empty
  if(nrow(rothc.event) == 0L){
    return(rothc.event)
  }

  # align time for amendment and crop events
  rothc.event[, time := time - min(time)]
  
  
  # sum multiple additives that are given at same time
  rothc.event <- rothc.event[,list(value = sum(value)),by = c('time','var','method')]
  
  # extend duration of event block and correction factors
  
  # add an unique ID
  rothc.event[,id := .I]
  
  ## extend crop table for the number of years
  period <- max(1, ceiling(max(rothc.event$time))) 
  rothc.event <- rothc.event[rep(id, each = ceiling(simyears / period))]
  
  ## update the time for all repetitions of rotation block
  rothc.event[,yr_rep := 1:.N, by = id]
  rothc.event[,year := (yr_rep - 1) * period]
  rothc.event[,time := year + time]
  
  # filter only the years for simulation
  rothc.event <- rothc.event[time <= simyears]
  
  # remove helper columns
  rothc.event[,c('id','year','yr_rep') := NULL]
  
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
#' 
#' @export
rc_input_event_crop <- function(crops){
 
   # Return empty crop table if no crops have been provided
  if(is.null(crops) || nrow(crops) == 0L){
    return(data.table(time = numeric(0), var = character(0), value = numeric(0), method = character(0)))
  }

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
  checkmate::assert_numeric(crops$cin_dpm, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(crops$cin_rpm, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_integerish(crops$year, any.missing = FALSE, len = arg.length)
  if (!"month" %in% names(crops)) crops[, month := NA_real_]
  
  
  # make internal copy
  dt <- copy(crops)
  
  # If month is not supplied, set to 9
  dt[, month := as.integer(month)]
  dt[is.na(month), month := 9]

  # setorder
  setorder(dt,year,month)
  
  # add cumulative time vector
  dt[,time := year + month/12]
  
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
#' This function calculates the timing of carbon inputs (kg C per ha) based on type of amendment application.

#' @param amendment (data.table) year, month (optional, defaults to 9), cin_hum, cin_dpm, and cin_rpm, cin_tot (optional)
#'
#' @details This function increases temporal detail for time series of C inputs of organic amendments.
#' The inputs for organic amendments are organised in the data.table amendment, where the carbon inputs has the unit kg C / ha.
#'
#' The output is an EVENT object.
#'
#' @export
rc_input_event_amendment <- function(amendment = NULL){
  
  # add visual bindings
  time = cin_hum = cin_rpm = cin_dpm = method = NULL
  
  # make local copy
  dt <- copy(amendment)
  
  # return empty event table if no amendment provided
  if(is.null(dt) || nrow(dt) == 0L){
    return(data.table(time = numeric(0), var = character(0), value = numeric(0), method = character(0)))
    }

  # do checks on the input of C due to organic amendments
  checkmate::assert_data_table(dt)
  required <- c('year','cin_hum','cin_dpm','cin_rpm')
  checkmate::assert_true(all(required %in% colnames(dt)))
  checkmate::assert_numeric(dt$cin_hum,lower = 0, upper = 100000,len = nrow(dt), any.missing = FALSE)
  if("cin_tot" %in% colnames(dt)){
  checkmate::assert_numeric(dt$cin_tot,lower = 0, upper = 100000,len = nrow(dt), any.missing = FALSE)
  }
  checkmate::assert_numeric(dt$cin_dpm,lower = 0, upper = 100000,len = nrow(dt), any.missing = FALSE)
  checkmate::assert_numeric(dt$cin_rpm,lower = 0, upper = 100000,len = nrow(dt), any.missing = FALSE)
  checkmate::assert_integerish(dt$year,len = nrow(dt), any.missing = FALSE)
  
  # If month is na, set to 9
  if (!"month" %in% names(dt)) dt[, month := NA_real_]
  dt[, month := as.integer(month)]
  dt[is.na(month), month := 9L]

  # add cumulative time vector
  dt[,time := year + month / 12]
  
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
