#' Calculate the monthly timing of carbon inputs for different fertilizer strategies
#'
#' This function calculates the timing of carbon inputs (kg C per ha) based on type of amendment application.

#' @param amendment (data.table) A table with the following column names: year, month, cin_tot, cin_hum, cin_dpm, and cin_rpm.
#' @param dt.time (data.table) Table containing all combinations of months and years in the simulation period, with columns year, month, and time
#'
#' @details This function increases temporal detail for time series of C inputs of organic amendments.
#' The inputs for organic amendments are organised in the data.table amendment, where the carbon inputs has the unit kg C / ha.
#' dt. time Must contain integerish columns \code{year} (>= 0) and \code{month} (1..12), numeric column \code{time}, 
#' and unique \code{(year, month)} pairs over the simulation window.
#'
#' The output is an EVENT object.
#'
#' @export
rc_input_event_amendment <- function(amendment = NULL, dt.time){
  
  # add visual bindings
  time = cin_hum = cin_rpm = cin_dpm = method = var = NULL
  
  # return empty event table if no amendment provided
  if(is.null(amendment) || nrow(amendment) == 0L){
    return(data.table(time = numeric(0), var = character(0), value = numeric(0), method = character(0)))
  }
  # make local copy
  dt <- copy(amendment)
  
  # do checks on the input of C due to organic amendments
  checkmate::assert_data_table(dt)
  required <- c('year', 'month', 'cin_hum','cin_dpm','cin_rpm')
  checkmate::assert_names(colnames(dt), must.include = required)
  checkmate::assert_integerish(dt$month, lower = 1, upper = 12, len = nrow(dt), any.missing = FALSE)
  checkmate::assert_numeric(dt$cin_hum,lower = 0, upper = 100000,len = nrow(dt), any.missing = FALSE)
  if("cin_tot" %in% colnames(dt)){
    checkmate::assert_numeric(dt$cin_tot,lower = 0, upper = 100000,len = nrow(dt), any.missing = FALSE)
  }
  checkmate::assert_numeric(dt$cin_dpm,lower = 0, upper = 100000,len = nrow(dt), any.missing = FALSE)
  checkmate::assert_numeric(dt$cin_rpm,lower = 0, upper = 100000,len = nrow(dt), any.missing = FALSE)
  checkmate::assert_integerish(dt$year,len = nrow(dt), any.missing = FALSE)
  
  # validate dt.time
  checkmate::assert_data_table(dt.time, any.missing = FALSE)
  checkmate::assert_names(colnames(dt.time), must.include = c('year','month','time'))
  checkmate::assert(
        !any(duplicated(dt.time[, paste(year, month)])),
        msg = "dt.time has duplicate (year, month) combinations"
  )
  
  # add cumulative time vector
  dt <- merge(dt.time, dt, by = c('year','month'), all.x = T)
  
  # Replace missing carbon inputs with 0 after aligning to time grid
  dt[is.na(cin_dpm), cin_dpm := 0]
  dt[is.na(cin_rpm), cin_rpm := 0]
  dt[is.na(cin_hum), cin_hum := 0]
  
  # select only those events that manure input occurs
  dt <- dt[cin_hum > 0 | cin_rpm > 0 | cin_dpm > 0]
  
  # select only relevant columns, rename them
  out <- dt[,list(CDPM = cin_dpm, CRPM = cin_rpm, CHUM = cin_hum,time = time)]
  
  # melt the output table
  out <- melt(out,id.vars = "time", variable.name = "var")
  
  # add method how RothC should treat the event
  out[, method := 'add']
  
  setorder(out, time, var)
  
  # return output
  return(out)
}