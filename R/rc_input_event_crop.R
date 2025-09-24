
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
  dt[,time := year + (month-1)/12]
  
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