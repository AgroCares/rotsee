
#' Calculate the crop rotation related C inputs of a field on monthly basis
#'
#' This function determines how much Carbon enters the soil throughout the year given the crop rotation plan.
#'
#' @param crops (data.table) Table containing the columns year, month, cin_dpm, and cin_rpm
#' @param dt.time (data.table) Table containing all combinations of months and years in the simulation period
#' 
#' @export
rc_input_event_crop <- function(crops, dt.time){
  
  # Return empty crop table if no crops have been provided
  if(is.null(crops) || nrow(crops) == 0L){
    return(data.table(time = numeric(0), var = character(0), value = numeric(0), method = character(0)))
  }
  
  # add visual bindings
  time = cin_dpm = cin_rpm = method = NULL
  
  # check inputs
  arg.length <- nrow(crops)
  
  # check crops input data.table
  checkmate::assert_data_table(crops, nrows = arg.length)
  checkmate::assert_names(names(crops), must.include = c('year','cin_dpm','cin_rpm'))
  checkmate::assert_numeric(crops$cin_dpm, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_numeric(crops$cin_rpm, lower = 0, any.missing = FALSE, len = arg.length)
  checkmate::assert_integerish(crops$year, any.missing = FALSE, len = arg.length)
  if (!"month" %in% names(crops)) crops[, month := NA_real_]
  crops[is.na(month), month := 9] # If month is not supplied, set to 9
  crops[, month := as.integer(month)]
  checkmate::assert_numeric(crops$month, lower = 1, upper = 12, any.missing = FALSE, len = arg.length)
  
  # check dt.time
  checkmate::assert_data_table(dt.time, min.rows = 1)
  checkmate::assert_names(names(dt.time), must.include = c('year','month','time'))
  checkmate::assert_integerish(dt.time$year, any.missing = FALSE)
  checkmate::assert_integerish(dt.time$month, lower = 1, upper = 12, any.missing = FALSE)
  checkmate::assert_numeric(dt.time$time, lower = 0, any.missing = FALSE)
  
  # make internal copy
  dt <- copy(crops)
  
  # setorder
  setorder(dt,year,month)
  
  # add cumulative time vector
  dt <- merge(dt.time, dt, by = c('year','month'), all.x = T)
  
  # replace missing carbon inputs by 0 after aligning to time grid
  dt[is.na(cin_dpm), cin_dpm := 0]
  dt[is.na(cin_rpm), cin_rpm := 0]
 
  # select only relevant columns as output for EVENT crop residue input
  # and select only those time steps where C input is bigger than zero
  out1 <- dt[cin_dpm > 0 | cin_rpm > 0,list(CDPM = cin_dpm, CRPM = cin_rpm,time = time)]
  
  # melt the output table
  out1 <- melt(out1,id.vars = "time", variable.name = "var")
  
  # add method how RothC should treat the event
  out1[, method := 'add']
  
  # ensure deterministic ordering
  setorder(out1, time, var)

  # return output
  return(out1)
}