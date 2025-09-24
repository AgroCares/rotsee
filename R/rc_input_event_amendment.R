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
  
  # return empty event table if no amendment provided
  if(is.null(amendment) || nrow(amendment) == 0L){
    return(data.table(time = numeric(0), var = character(0), value = numeric(0), method = character(0)))
  }
  # make local copy
  dt <- copy(amendment)
  
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
  
  # Check month column, and if NA set to 9
  if (!"month" %in% names(dt)) dt[, month := NA_real_]
  dt[, month := as.numeric(month)]
  dt[is.na(month), month := 9L]
  checkmate::assert_numeric(dt$month, lower = 1, upper = 12, any.missing = FALSE, len = nrow(dt))
  
  # add cumulative time vector
  dt[,time := year + (month - 1)/ 12]
  
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