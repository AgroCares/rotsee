#' Estimate the rate modifying factors for the RothC modelling
#'
#' This function prepares the rate modifying factors for RothC given crop cover
#'
#' @param dt (data.table) Table with crop rotation and related crop properties for Carbon input.
#' @param B_LU_BRP (numeric) The crop code
#' @param B_DEPTH (numeric) Depth of the cultivated soil layer (m), simulation depth. Default set to 0.3.
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%)
#' @param simyears (numeric) Amount of years for which the simulation should run, default: 50 years
#' @param cf_yield (numeric) A yield correction factor (fraction) if yield is higher than regional average
#'
#' @details
#' To run this function, the dt requires as input: B_LU (a crop id), B_LU_NAME (a crop name, optional), B_LU_EOM (the effective organic matter content, kg/ha), B_LU_EOM_RESIDUE (the effective organic matter content for crop residues, kg/ha), and the B_LU_HC (the humification coeffient,-).
#' if dt is NULL, then the crop input will be prepared using function \link{rc_input_scenario} using scenario 'BAU'
#'
#' @export
rc_input_rmf <- function(dt = NULL,B_LU_BRP = NULL, B_DEPTH = 0.3, A_CLAY_MI, simyears,cf_yield){
  
  # add visual bindings
  B_LU = crop_name = M_RENEWAL = B_LU_MAKKINK = B_LU_NAME = M_GREEN_TIMING = NULL
  mcf = crop_cover = crflt = time = cf_temp = temp = tsmdmax = NULL
  tsmdmax_cor = et_act = et_pot = smd = prec = hv = acc_smd = acc_smd2 = cf_moist = cf_soilcover = NULL
  cf_renewal = cf_combi = id = yr_rep = NULL
  
  # check B_LU_BRP or crop table
  checkmate::assert_integerish(B_LU_BRP, any.missing = FALSE, null.ok = TRUE, min.len = 1)
  checkmate::assert_subset(B_LU_BRP, choices = unique(rotsee::rc_crops$crop_code), empty.ok = TRUE)
  checkmate::assert_data_table(dt,null.ok = TRUE)
  checkmate::assert_true(!(is.null(dt) & is.null(B_LU_BRP)))
  checkmate::assert_numeric(cf_yield,lower = 0.1, upper = 2.0, any.missing = FALSE,len = 1)
  
  # set default crop table in case that dt is missing
  if(is.null(dt) & !is.null(B_LU_BRP)){
    
    rs <- rc_input_scenario(B_LU_BRP = B_LU_BRP, scen = 'BAU')
    dt <- rs$rotation
  }
  
  # create a default weather database for Dutch conditions
  dt.weather <- data.table(month = 1:12,
                           temp = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                           prec = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                           et_pot = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                           et_act = NA_real_)
  
  # prepare makkink file
  rc.crops <- as.data.table(rotsee::rc_crops)
  
  dt.mak <- as.data.table(rotsee::rc_makkink)
  dt.mak <- melt(dt.mak,id.vars = 'B_LU_MAKKINK', variable.name = 'month',value.name = "mcf",variable.factor = FALSE)
  dt.mak[,month := as.integer(month)]
  
  # check crops input data
  checkmate::assert_data_table(dt)
  checkmate::assert_true(sum(c('M_GREEN_TIMING','M_IRRIGATION','M_CROPRESIDUE') %in% colnames(dt)) == 3)
  checkmate::assert_numeric(dt$cf_yield,lower = 0, upper = 2.0, any.missing = FALSE)
  checkmate::assert_character(dt$M_GREEN_TIMING, any.missing = FALSE)
  checkmate::assert_subset(dt$M_GREEN_TIMING,c('august','september','october','november','never'))
  checkmate::assert_logical(dt$M_CROPRESIDUE,any.missing = FALSE)
  checkmate::assert_logical(dt$M_IRRIGATION,any.missing = FALSE)
  
  # add crop name to input
  dt <- merge(dt,
              rc.crops[, list(B_LU, B_LU_NAME = crop_name, B_LU_MAKKINK)],
              by = 'B_LU',
              all.x = TRUE)
  
  # update input for mandatory catch crops (after maize and potato on sandy soils) and set NA for grassland
  if(A_CLAY_MI <20){ dt[grepl('mais|aardappel',B_LU_NAME) & grepl('^nl_',B_LU), M_GREEN_TIMING := 'october'] }
  dt[grepl('gras|bieten, suiker|bieten, voeder',B_LU_NAME) & grepl('^nl_',B_LU), M_GREEN_TIMING := 'never']
  
  # add crop cover and makkink correction factor
  
  # extend the crop table with months
  dt <- dt[rep(1:.N,12)]
  dt[,month := 1:.N,by='year']
  
  # add makkink correction factor
  dt <- merge(dt,dt.mak, by = c('B_LU_MAKKINK','month'),all.x = TRUE)
  
  # add median Makkink correction factor for non-Dutch crops
  dt[is.na(mcf) & month %in% c(1:3,10:12), mcf := 0.36]
  dt[is.na(mcf) & month == 4, mcf := 0.4]
  dt[is.na(mcf) & month == 5, mcf := 0.66]
  dt[is.na(mcf) & month %in% 6:7, mcf := 1.0]
  dt[is.na(mcf) & month == 8, mcf := 1.06]
  dt[is.na(mcf) & month == 9, mcf := 0.95]
  
  # add crop cover (crops crowing = 1, no crop = 0)
  dt[,crop_cover := fifelse(mcf>0.36,1,0)]
  
  # column names to be updated
  cols <- c("B_LU_NAME","crop_cover","mcf")
  
  # select the years before winter cereals are grown as main crop or when catch crop grown in spring
  year_wc <- dt[grepl('winter',B_LU_NAME) & grepl('tarwe|gerst',B_LU_NAME),unique(pmax(1,year - 1))]
  year_cc <- dt[M_GREEN_TIMING != 'never',unique(pmin(year + 1,max(1,year)))]
  
  # update B_LU_NAME, crop_cover and mcf for winter crops and catch crops
  
  # update winter crop in winter months
  dt[year %in% year_wc & month == 10, c(cols) := list("winter cereal", 1, 0.5)]
  dt[year %in% year_wc & month > 10, c(cols) := list("winter cereal", 1, 0.6)]
  
  # update catch crop in winter months
  dt[M_GREEN_TIMING != 'never', crflt := 1]
  dt[crflt == 1 & month == 10, c(cols):=list("catch crop",1,0.74)]
  dt[crflt == 1 & month == 11, c(cols):=list("catch crop",1,0.64)]
  dt[crflt == 1 & month == 12, c(cols):=list("catch crop",1,0.60)]
  dt[,crflt := NULL]
  
  # update catch crop in next year
  dt[month < 4 & year %in% year_cc,c(cols) := list("catch crop",1,0.6)]
  
  # adapt mcf when crop yield is different than average
  dt[!grepl('catch crop',B_LU_NAME), mcf := mcf * cf_yield]
  
  # sort dt on the original order
  setorder(dt,year,month)
  
  # add time
  dt[, time := year + (month - 1) / 12 - 1]
  
  # select relevant outputs
  dt <- dt[,list(time,mcf,crop_cover,M_RENEWAL)]
  
  
  # add rate modifiying factors
  
  # select crop cover and Makkink correction factor
  dt.cc <- copy(dt)
  
  # combine rotation data in one table with monthly means, and ensure that mcf is smaller than 2
  dt.cc[,year := floor(time)]
  dt.cc[,month := round(12 * (dt$time - floor(dt$time)) + 1)]
  dt.cc[,mcf := pmin(2,mcf)]
  
  # add correction factors for wheather
  dt.weather[, cf_temp :=  47.9/(1+exp(106/(temp + 18.3)))]
  
  # add maximal top soil moisture deficit (TSMD) and bare soil moisture deficit
  dt.weather[, tsmdmax := -(20 + 1.3 * A_CLAY_MI - 0.01 * (A_CLAY_MI^2)) * B_DEPTH / 0.23]
  
  # add weather data and reorder on time
  dt.cc <- merge(dt.cc,dt.weather,by='month')
  setorder(dt.cc,time)
  
  # correct for bare SMD when there is no crop growing
  dt.cc[, tsmdmax_cor := fifelse(crop_cover==1,tsmdmax,tsmdmax/1.8)]
  
  # add actual evapo-transpiration and soil moisture deficit
  dt.cc[is.na(et_act),et_act := et_pot * mcf]
  dt.cc[,smd := prec- et_act]
  
  # add accumulated soil moisture deficit, uncorrected
  dt.cc[, hv := pmin(1,cumsum(fifelse(smd<0,1,0))),by='year']
  dt.cc[, acc_smd := hv * smd]
  
  # helper function to estimate soil moisture deficit per year
  hfun <- function(x,xmax){o=x;for(i in 2:length(x)){o[i] <- min(0,max(xmax[i], x[i] + x[i-1]))};return(abs(o))}
  
  # estimate soil moisture deficit, corrected
  dt.cc[,acc_smd2 := hfun(acc_smd,tsmdmax_cor),by='year']
  dt.cc[,tsmdmax := abs(tsmdmax)]
  
  # add rate modifying factor for moisture
  dt.cc[,cf_moist := fifelse(acc_smd2 < 0.444 * tsmdmax,1, pmax(0.2,1 - 0.8 * (acc_smd2 - 0.44*tsmdmax)/ (tsmdmax - 0.444*tsmdmax)))]
  
  # add rate modifying factor for soil cover
  dt.cc[,cf_soilcover := fifelse(crop_cover==1,0.6,1)]
  
  # add rate modifying factor for grassland renewal
  renew.year <- unique(floor(dt.cc[M_RENEWAL == TRUE, time])) + 1
  dt.cc[,cf_renewal := fifelse(year %in% renew.year & month == 1,1,0)]
  
  # add combined rate modifying factor
  dt.cc[,cf_combi := cf_temp * cf_moist * cf_soilcover]
  
  # order the output on time
  setorder(dt.cc,time)
  
  # select only relevant variables for rate modifying factors
  rothc.mf <- dt.cc[,list(time = time,a = cf_temp, b = cf_moist, c = cf_soilcover, d = cf_renewal, abc = cf_combi)]
  
  # expand the input file
  
  # add an unique ID
  rothc.mf[,id := .I]
  
  ## extend crop table for the number of years
  rothc.mf <- rothc.mf[rep(id, each = ceiling(simyears / max(time)))]
  
  ## update the time for all repetitions of rotation block
  rothc.mf[,yr_rep := 1:.N, by = id]
  rothc.mf[,year := (yr_rep - 1) * ceiling(max(time)), by = yr_rep]
  rothc.mf[,time := year + time]
  
  # filter only the years for simulation
  rothc.mf <- rothc.mf[round(time) <= simyears]
  
  # remove helper columns
  rothc.mf[,c('id','year','yr_rep') := NULL]
  
  # order
  setorder(rothc.mf,time)
  
  # Update time to match events
  rothc.mf[,time := time + 1/12]
  
  # derive rate modifying factor for full simulation period
  
  # calculate interpolation for correction factors
  abc <- stats::approxfun(x = rothc.mf$time,y = rothc.mf$abc, method = "linear",rule=2)
  d <- stats::approxfun(x = rothc.mf$time,y = rothc.mf$d, method = "constant",f=1,rule=2)
  
  # calculate correction factor for soil structure
  R1 <- 1/((1.67*(1.85+1.6*exp(-0.0786*A_CLAY_MI)))+1)
  
  # combine RothC input parameters
  rothc.parms <- list(R1 = R1, abc = abc, d = d)
  
  # return output
  return(rothc.parms)
  
}