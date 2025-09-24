#' Combine all EVENT data needed for RothC modelling
#'
#' This function combines required inputs into a data.table that is needed as input for the RothC model.
#'
#' @param crops (data.table) Table containing carbon inputs of DPM and RPM from crops, calculated in rc_input_event_crop
#' @param amendment (data.table) Table containing carbon inputs of DPM, RPM, and HUM from amendments, calculated in rc_input_event_amendment

#'
#' @export
rc_input_events <- function(crops,amendment){
  
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

