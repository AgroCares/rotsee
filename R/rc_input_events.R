#' Combine all EVENT data needed for RothC modelling
#'
#' This function combines required inputs into a data.table that is needed as input for the RothC model.
#'
#' @param crops (data.table) Table with crop rotation, cultivation management, year and potential Carbon inputs.
#' @param amendment (data.table) A table with the following column names: year, month, cin_tot, cin_hum, cin_dpm, cin_rpm and the fraction eoc over p (fr_eoc_p). Month is optional.
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#' @param simyears (numeric) Amount of years for which the simulation should run, default: 50 years
#'
#' @export
rc_input_events <- function(crops,amendment,A_CLAY_MI,simyears){
  
  # add visual bindings
  id = time = yr_rep = NULL
  
  # estimate default crop rotation plan, the building block
  event.crop <- rc_input_event_crop(crops = crops, A_CLAY_MI)
  
  # estimate Carbon input via manure, compost and organic residues
  event.man <- rc_input_event_amendment(crops = crops,amendment = amendment)
  
  # create event
  rothc.event <- rbind(event.crop,event.man)
  
  # sum multiple additives that are given at same time
  rothc.event <- rothc.event[,list(value = sum(value)),by = c('time','var','method')]
  
  # extend duration of event block and correction factors
  
  # add an unique ID
  rothc.event[,id := .I]
  
  ## extend crop table for the number of years
  rothc.event <- rothc.event[rep(id, each = ceiling(simyears / max(time)))]
  
  ## update the time for all repetitions of rotation block
  rothc.event[,yr_rep := 1:.N, by = id]
  rothc.event[,year := (yr_rep - 1) * ceiling(max(time)), by = yr_rep]
  rothc.event[,time := year + time]
  
  # filter only the years for simulation
  rothc.event <- rothc.event[round(time) <= simyears]
  
  # remove helper columns
  rothc.event[,c('id','year','yr_rep') := NULL]
  
  # order
  setorder(rothc.event,time)
  
  # return output
  return(rothc.event)
}
