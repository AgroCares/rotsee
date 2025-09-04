#' Estimate the rate modifying factors for the RothC modelling
#'
#' This function prepares the rate modifying factors for RothC given crop cover
#'
#' @param dt (data.table) Table with crop rotation and related crop properties for Carbon input.
#' @param B_DEPTH (numeric) Depth of the cultivated soil layer (m), simulation depth. Default set to 0.3.
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param dt.weather (data.table) Data table of monthly weather
#' @param dt.time (data.table) table with all combinations of year and month in the simulation period
#'
#' @details
#' dt: crop rotation table
#' contains at least the following columns:
#' * B_LU_START (date), start of crop growth
#' * B_LU_END (DATE), end of crop growth
#' 
#' dt.weather: weather table
#' contains the following columns:
#' * year (optional)
#' * month
#' * W_TEMP_MEAN_MONTH
#' * W_PREC_SUM_MONTH
#' * W_ET_POT_MONTH
#' * W_ET_ACT_MONTH
#'
#' @export
rc_input_rmf <- function(dt = NULL, B_DEPTH = 0.3, A_CLAY_MI,  dt.weather, dt.time){
  
  # add visual bindings
  B_LU_START = B_LU_END = crop_cover = time = cf_temp = W_TEMP_MEAN_MONTH = NULL
  tsmdmax = tsmdmax_cor = W_ET_ACT_MONTH = W_ET_POT_MONTH = smd = acc_smd = NULL
  W_PREC_SUM_MONTH = cf_moist = cf_soilcover = cf_combi = id = yr_rep = NULL
  
  # Input tables
  checkmate::assert_data_table(dt,null.ok = TRUE)
  checkmate::assert_true(all(c('B_LU_START', 'B_LU_END') %in% colnames(dt)))
  checkmate::assert_date(as.Date(dt$B_LU_START), any.missing = F)
  checkmate::assert_date(as.Date(dt$B_LU_END), any.missing = F)
  checkmate::assert_data_table(dt.weather, null.ok = FALSE)
  checkmate::assert_subset(colnames(dt.weather), choices = c("year", "month", "W_TEMP_MEAN_MONTH", "W_PREC_SUM_MONTH", "W_ET_POT_MONTH", "W_ET_ACT_MONTH"))

  # Establish months of crop cover based on start and end of crop rotation
  dt.growth <- dt[, {
    
    # Create a sequence of year-month combinations when crops are growing
    seq_dates <- seq.Date(as.Date(B_LU_START), as.Date(B_LU_END), by = 'month')
    
    # Extract year and month
    list(year = year(seq_dates), month = month(seq_dates), crop_cover = 1)
  }, by = list(B_LU_START,B_LU_END)] 

 
  # Create a complete set of year-month combinations
  dt.full_year <- CJ(year = min(year(dt$B_LU_START)):max(year(dt$B_LU_END)), month = 1:12)
 
  # Merge and fill missing crop_cover values with 0
  dt.crop_cover <- merge(dt.full_year, dt.growth, by = c("year", "month"), all.x = TRUE)[, crop_cover := fifelse(is.na(crop_cover), 0, crop_cover)]
  dt.crop_cover <- unique(dt.crop_cover[,list(year,month,crop_cover)])
  
  # Make selection of dates between start and end date
  dt.time <-  dt.full_year[,date := as.Date(paste(year, month, "01", sep = "-"))]
    
  dt.time <- dt.time[date >= as.Date(paste(year(rothc_parms$start_date), month(rothc_parms$start_date), "01", sep = "-")) &
                        date <= as.Date(paste(year(rothc_parms$end_date), month(rothc_parms$end_date), "01", sep = "-"))]
  dt.time[,date := NULL]
  
  # Format time
  dt.time[, time := .I / 12 - 1/12]
  
  # Select relevant rows for crop cover and weather
  dt.crop_cover <- merge(dt.time, dt.crop_cover, by = c('year','month'))

  weather <- merge(dt.time, dt.weather, by = 'month', all.x=TRUE)
 
  # combine weather and crop cover data
  dt <- merge(weather, dt.crop_cover, by = c('time', 'year', 'month'))
  
  # Add soil data for rmf calculation
  dt[, B_DEPTH := B_DEPTH]
  dt[, A_CLAY_MI := A_CLAY_MI]

  # Add rate modifying factors
  
  # add correction factor for temperature
  dt[, cf_temp :=  47.9/(1+exp(106/(W_TEMP_MEAN_MONTH + 18.3)))]
  
  # add correction factor for top soil moisture deficit
  # Calculate maximum top soil moisture deficit
  dt[, tsmdmax := -(20 + 1.3 * A_CLAY_MI - 0.01 * (A_CLAY_MI^2)) * B_DEPTH / 0.23]

  # correct maximum top soil moisture deficit for bare soil
  dt[, tsmdmax_cor := fifelse(crop_cover==1,tsmdmax,tsmdmax/1.8)]

  # Calculate actual evapotranspiration for months where only potential is provided (general rothc calculation)
  dt[is.na(W_ET_ACT_MONTH), W_ET_ACT_MONTH := W_ET_POT_MONTH * 0.75]

  # Calculate the monthly soil moisture deficit
  dt[,smd := W_PREC_SUM_MONTH - W_ET_ACT_MONTH]

  
  # Calculate the accumulated soil moisture deficit
  dt[, acc_smd := {
    # Create filler column of the length of the data table
    x = numeric(length(smd))
    
    # Base initial deficit on deficit in the same period yet for the next year
    x[1] = smd[1]
    
    # Apply constraints to initial value
    x[1] = pmin(0, pmax(x[1], tsmdmax_cor[1]))
    
    # Loop through remaining values
    for (i in 2:.N) {
      x[i] = x[i-1] + smd[i]
      x[i] = pmin(0, pmax(x[i], tsmdmax_cor[i]))
    }
    # return output
    return(x)
  }]
 
  # add rate modifying factor for moisture
  dt[,cf_moist := fifelse(acc_smd > 0.444 * tsmdmax_cor,1, pmax(0.2, 0.2 + (1 - 0.2) * (tsmdmax_cor - acc_smd)/ (tsmdmax_cor - 0.444*tsmdmax_cor)))]
 
  
  # add rate modifying factor for soil cover
  dt[,cf_soilcover := fifelse(crop_cover==1,0.6,1)]
 
  # add combined rate modifying factor
  dt[,cf_combi := cf_temp * cf_moist * cf_soilcover]
  
  # order the output on time
  setorder(dt,time)
  
  # select only relevant variables for rate modifying factors
  rothc.mf <- dt[,list(time = time,a = cf_temp, b = cf_moist, c = cf_soilcover, abc = cf_combi)]

  # derive rate modifying factor for full simulation period
  # calculate interpolation for correction factors
  abc <- stats::approxfun(x = rothc.mf$time,y = rothc.mf$abc, method = "linear",rule=2)
  
  # calculate correction factor for soil structure
  R1 <- 1/((1.67*(1.85+1.6*exp(-0.0786*A_CLAY_MI)))+1)
 
  # combine RothC input parameters
  rothc.parms <- list(R1 = R1, abc = abc, time = rothc.mf$time)
  
  # return output
  return(rothc.parms)
  
}