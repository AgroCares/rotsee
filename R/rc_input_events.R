#' Combine all EVENT data needed for RothC modelling
#'
#' This function combines required inputs into a data.table that is needed as input for the RothC model.
#'
#' @param crops (data.table) Table containing carbon inputs of DPM and RPM from crops, calculated in rc_input_event_crop
#' @param amendment (data.table) Table containing carbon inputs of DPM, RPM, and HUM from amendments, calculated in rc_input_event_amendment
#' @param simyears (numeric) Amount of years for which the simulation should run, Default: 50 years
#'
#' @export
rc_input_events <- function(crops, amendment, simyears = 50){
  
  # add visual bindings
  id = time = yr_rep = NULL
  
  # check parameters
  checkmate::assert_numeric(simyears,lower = 0.1,  any.missing = FALSE)
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


