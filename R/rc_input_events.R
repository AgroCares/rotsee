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

#' Calculate the crop rotation related C inputs of a field on monthly basis
#'
#' This function determines how much Carbon enters the soil throughout the year given the crop rotation plan.
#'
#' @param crops (data.table) Table with crop rotation, crop management measures, year and potential Carbon inputs.
#' @param A_CLAY_MI (numeric) The clay content of the soil (\%).
#'
#' @export
rc_input_event_crop <- function(crops,A_CLAY_MI){
  
  # add visual bindings
  crop_name = B_LU = NULL
  M_GREEN_TIMING = M_CROPRESIDUE = green_eom = NULL
  crflt = cin_dpm = cin_crop_dpm = cin_res_dpm = cin_rpm = cin_crop_rpm = cin_res_rpm = NULL
  cin_crop = tcf = method = cf_yield = crop_code = time = NULL
  
  # check inputs
  arg.length <- nrow(crops)
  
  # check crops input data.table
  checkmate::assert_data_table(crops,nrows = arg.length)
  checkmate::assert_true(sum(c('M_GREEN_TIMING','M_CROPRESIDUE','cf_yield') %in% colnames(crops)) == 3)
  checkmate::assert_numeric(crops$cf_yield,lower = 0, upper = 2.0, any.missing = FALSE, len = arg.length)
  checkmate::assert_character(crops$M_GREEN_TIMING, any.missing = FALSE, len = arg.length)
  checkmate::assert_subset(crops$M_GREEN_TIMING, choices = c("august","september", "october","november","never"))
  checkmate::assert_logical(crops$M_CROPRESIDUE,any.missing = FALSE, len = arg.length)
  checkmate::assert_integerish(crops$year,any.missing = FALSE, len = arg.length)
  
  # make internal copy
  dt <- copy(crops)
  
  # get internal copies of tables
  cc.crop <- as.data.table(rotsee::rc_crops)
  cc.crop[,B_LU := paste0('nl_',crop_code)]
  cc.crop[,fr_dpm_rpm := fifelse(B_LU_HC < 0.92, -2.174 * B_LU_HC + 2.02, 0)]
  cc.crop[is.na(fr_dpm_rpm), fr_dpm_rpm := 1.44]
  
  # add crop names and catch crop characteristics to table
  cols <- c(colnames(cc.crop)[!colnames(cc.crop) %in% colnames(dt)],'B_LU')
  dt <- merge(dt,cc.crop[,mget(cols)],by = 'B_LU',all.x = TRUE)
  
  # adjust crop rotation plan, and extend to months
  
  # update input for mandatory catch crops (after maize and potato on sandy soils) and set never for grassland
  if(A_CLAY_MI <20){ dt[grepl('mais|aardappel',crop_name) & grepl('^nl_',B_LU), M_GREEN_TIMING := 'october'] }
  dt[grepl('gras|bieten, suiker|bieten, voeder|zetmeel',crop_name) & grepl('^nl_',B_LU), M_GREEN_TIMING := 'never']
  
  # make table with catch crops and update year, as catch crops will be incorporated next year
  dt.green <- dt[,list(year,M_GREEN_TIMING)]
  dt.green[,year := year + 1]
  
  # set crop carbon inputs from catch crops
  dt.green[M_GREEN_TIMING == 'august', green_eom := 900]
  dt.green[M_GREEN_TIMING == 'september', green_eom := 500]
  dt.green[M_GREEN_TIMING == 'october', green_eom := 300]
  dt.green[M_GREEN_TIMING == 'november', green_eom := 0]
  dt.green[,M_GREEN_TIMING := NULL]
  
  
  # extend the crop table with months
  dt <- dt[rep(1:.N,12)]
  dt[,month := 1:.N,by='year']
  
  # select the years before winter cereals are grown as main crop or when catch crops grown in spring
  year_wc <- dt[grepl('^nl_',B_LU) & grepl('winter',crop_name) & grepl('tarwe|gerst',crop_name),
                unique(pmax(1,year - 1))]
  year_cc <- dt[grepl('^nl_',B_LU) & !grepl('never',M_GREEN_TIMING),
                unique(pmin(year + 1,max(1,year)))]
  
  # update crop_name for winter crops and catch crops
  dt[grepl('^nl_',B_LU) & year %in% year_wc & month >= 10, crop_name := "winter cereal"]
  dt[grepl('^nl_',B_LU) & !grepl('never',M_GREEN_TIMING), crflt := 1]
  dt[crflt == 1 & month >= 10, crop_name := "catch crop"]
  # if main crop is winter cereal then first months are the winter crop
  dt[grepl('^nl_',B_LU) & month < 4 & year %in% year_cc, crop_name := "catch crop"]
  dt[,crflt := NULL]
  
  # estimate total EOC input via crop and residue
  dt[, cin_crop_dpm :=  (B_LU_EOM * 0.5 / B_LU_HC) *  fr_dpm_rpm / (1 + fr_dpm_rpm)]
  dt[, cin_crop_rpm :=  (B_LU_EOM * 0.5 / B_LU_HC) *  1 / (1 + fr_dpm_rpm)]
  dt[, cin_res_dpm :=  (B_LU_EOM_RESIDUE * 0.5 / B_LU_HC) *  fr_dpm_rpm / (1 + fr_dpm_rpm)]
  dt[, cin_res_rpm :=  (B_LU_EOM_RESIDUE * 0.5 / B_LU_HC) *  1 / (1 + fr_dpm_rpm)]
  
  # add total crop EOC input (kg C/ ha) for the RothC pools, uncorrected for cf_yield
  dt[,cin_dpm := cin_crop_dpm + fifelse(M_CROPRESIDUE==TRUE, cin_res_dpm, 0)]
  dt[,cin_rpm := cin_crop_rpm + fifelse(M_CROPRESIDUE==TRUE, cin_res_rpm, 0)]
  
  # the carbon input from crop and crop residue is only input at harvest for arable crops(assume month = 9) and grassland during cuts
  dt[grepl('^nl_',B_LU) & !grepl('grasland',crop_name) ,cin_dpm := fifelse(month == 9,cin_dpm,0)]
  dt[grepl('^nl_',B_LU) & !grepl('grasland',crop_name) ,cin_rpm := fifelse(month == 9,cin_rpm,0)]
  dt[grepl('^nl_',B_LU) & grepl('grasland',crop_name) ,cin_dpm := fifelse(month  %in% c(4,5,6,7,9),cin_dpm/5,0)]
  dt[grepl('^nl_',B_LU) & grepl('grasland',crop_name) ,cin_rpm := fifelse(month  %in% c(4,5,6,7,9),cin_rpm/5,0)]
  
  # add yield correction
  dt[,cin_dpm := cf_yield * cin_dpm]
  dt[,cin_rpm := cf_yield * cin_rpm]
  
  # add relevant timings for grassland clippings (10% from DM yield)
  dt[grepl('^nl_',B_LU) & grepl('grasland',crop_name),crflt := 1]
  dt[crflt == 1 & month == 4, cin_crop := 4000 * 0.5 * 0.1]
  dt[crflt == 1 & month == 5, cin_crop := 2500 * 0.5 * 0.1]
  dt[crflt == 1 & month == 6, cin_crop := 1750 * 0.5 * 0.1]
  dt[crflt == 1 & month == 7, cin_crop := 1750 * 0.5 * 0.1]
  dt[crflt == 1 & month == 9, cin_crop := 2500 * 0.5 * 0.1]
  
  # update DPM and RPM inputs for grassland clippings estimate inputs for DPM and RDM
  dt[crflt == 1,cin_dpm := cin_dpm + cin_crop * cf_yield * 1.2591 / (1 + 1.2591)]
  dt[crflt == 1,cin_rpm := cin_rpm + cin_crop * cf_yield * 1 / (1 + 1.2591)]
  dt[,crflt := NULL]
  
  # add carbon crop residue from catch crops
  dt <- merge(dt,dt.green, by = 'year', all.x = TRUE)
  dt[grepl('^nl_',B_LU) & grepl('catch crop',crop_name) & month == 3,crflt := 1]
  dt[crflt == 1, cin_dpm := (green_eom * 0.5 / 0.31) * cf_yield * 1.35 / (1 + 1.35)]
  dt[crflt == 1, cin_rpm := (green_eom * 0.5 / 0.31) * cf_yield * 1 / (1 + 1.35)]
  dt[,crflt := NULL]
  
  # setorder
  setorder(dt,year,month)
  
  # add cumulative time vector
  dt[,time := year + month/12 - min(year)]
  
  # select only relevant columns as output for EVENT crop residue input
  # and select only those time steps where C input is bigger than zero
  out1 <- dt[cin_dpm > 0 ,list(CDPM = cin_dpm,CRPM = cin_rpm,time = time)]
  
  # melt the output table
  out1 <- melt(out1,id.vars = "time", variable.name = "var")
  
  # add method how RothC should treat the event
  out1[, method := 'add']
  
  # return output
  return(out1)
}

#' Calculate the monthly timing of carbon inputs for different fertilizer strategies
#'
#' This function calculates the timing of carbon inputs (kg C per ha) based on type of organic matter amendments and land use.
#'
#' @param crops (data.table) Table with crop rotation, year and potential Carbon inputs.
#' @param amendment (data.table) A table with the following column names: year, month, cin_tot, cin_hum, cin_dpm, cin_rpm and the fraction eoc over p (fr_eoc_p). Month is optional.
#'
#' @details This function increases temporal detail for time series of C inputs of organic amendments.
#' The inputs for organic amendments are organised in the data.table amendment, where the carbon inputs has the unit kg C / ha.
#'
#' The output is an EVENT object.
#'
#' @export
rc_input_event_amendment <- function(crops,amendment = NULL){
  
  # add visual bindings
  B_LU = B_LU_NAME = p_cat = fre_eoc_p = crflt = tcf = NULL
  cin_hum = cin_rpm = cin_dpm = method = crop_code = crop_name = NULL
  fr_eoc_p = time = NULL
  
  # make local copy and add relevant variables if missing
  if(!is.null(amendment)){
    
    # make local copy
    dt <- copy(amendment)
    
    # set column names to lower case
    setnames(dt,tolower(colnames(dt)))
    
    # do check if C inputs per pool needs to be calculated
    check <- all(c('p_name','year','cin_tot','cin_hum','cin_dpm','cin_rpm','fr_eoc_p') %in% colnames(dt))
    
    if(check == FALSE){
      
      # add dpm-rmp ratio
      dt[,fr_dpm_rpm := fifelse(p_hc < 0.92, -2.174 * p_hc + 2.02, 0)]
      
      # estimate total Carbon input per crop and year (kg product * % organic matter * C-fraction = kg C / ha)
      dt[, cin_tot := p_dose * p_om * 0.01 * 0.5]
      
      # estimate C input for DPM, RDM and HUM pool
      dt[, cin_hum := 0.02 * cin_tot]
      dt[, cin_dpm := (1 - 0.02) * cin_tot * fr_dpm_rpm/ (1 + fr_dpm_rpm)]
      dt[, cin_rpm := (1 - 0.02) * cin_tot - cin_dpm]
      
      # fractie EOC per unit p
      dt[,fr_eoc_p := p_om * p_hc * 0.5 / p_p2o5]
      
      # select only columns that are relevant here
      dt <- dt[,.(year,month,cin_tot,cin_hum,cin_dpm,cin_rpm,fr_eoc_p)]
    }
    
  }
  
  
  # make default crop amendment data.table when dt = NULL
  if(is.null(dt)){dt <- data.table(year = crops[1,year], month = 1, cin_tot = 0, cin_hum = 0,
                                   cin_dpm = 0, cin_rpm = 0, fr_eoc_p = 10)}
  
  # do checks on the crop list
  checkmate::assert_data_table(crops)
  checkmate::assert_true('B_LU' %in% colnames(crops))
  checkmate::assert_true('year' %in% colnames(crops))
  
  # do checks on the input of C due to organic amendments
  checkmate::assert_data_table(dt)
  checkmate::assert_subset(colnames(dt),
                           c('year','month','p_name','cin_tot','cin_hum','cin_dpm','cin_rpm','fr_eoc_p'),
                           empty.ok = FALSE)
  checkmate::assert_numeric(dt$cin_hum,lower = 0, upper = 100000,len = nrow(dt))
  checkmate::assert_numeric(dt$cin_tot,lower = 0, upper = 100000,len = nrow(dt))
  checkmate::assert_numeric(dt$cin_dpm,lower = 0, upper = 100000,len = nrow(dt))
  checkmate::assert_numeric(dt$cin_rpm,lower = 0, upper = 100000,len = nrow(dt))
  checkmate::assert_numeric(dt$fr_eoc_p,lower = 0, upper = 250,len = nrow(dt))
  checkmate::assert_integerish(dt$year,len = nrow(dt))
  
  # load internal tables
  cc.crop <- as.data.table(rotsee::rc_crops)
  cc.crop[,B_LU := paste0('nl_',crop_code)]
  
  # merge table with amendment c input with crop name
  dt <- merge(dt, crops[,list(B_LU,year)], by='year')
  dt <- merge(dt, cc.crop[,list(B_LU,B_LU_NAME=crop_name)],by='B_LU')
  
  # add manure category depending on eoc-to-p ratio
  # assuming that soil improving products (high ratio) are incorporated in autumn, and others in spring
  dt[,p_cat := fifelse(fr_eoc_p > 20, 'autumn','spring')]
  
  # Sum C inputs over p_cat per year
  cols <- c('cin_hum','cin_dpm','cin_rpm')
  dt <- dt[,lapply(.SD,function(x) sum(x)),.SDcols = cols,by = c('year','p_cat','B_LU','B_LU_NAME')]
  
  # add monthly redistribution
  if(!'month' %in% colnames(dt)){
    
    # extend the crop table with months
    dt2 <- copy(dt)
    dt2 <- dt2[rep(1:.N,12)]
    dt2[,month := 1:.N,by=c('year','p_cat')]
    
    # add timings for grassland for three kind of amendments
    dt2[grepl('^nl_',B_LU) & grepl('gras',B_LU_NAME) & p_cat == 'spring', crflt := 1]
    dt2[crflt == 1, tcf := 0]
    dt2[crflt == 1 & month == 2, tcf := 0.35]
    dt2[crflt == 1 & month == 5, tcf := 0.25]
    dt2[crflt == 1 & month == 6, tcf := 0.10]
    dt2[crflt == 1 & month == 7, tcf := 0.10]
    dt2[crflt == 1 & month == 9, tcf := 0.20]
    dt2[,crflt := NULL]
    dt2[grepl('^nl_',B_LU) & grepl('gras',B_LU_NAME) & p_cat == 'autumn', tcf := fifelse(month==10,1,0)]
    
    # add timings for grassland not in the Netherlands
    dt2[grepl('gras',B_LU_NAME) & !grepl('nl_',B_LU), tcf := fifelse(month %in% c(4,6,8),1/3,0)]
    
    # add timings for non grassland systems
    dt2[!grepl('gras',B_LU_NAME) & p_cat == 'spring', tcf := fifelse(month == 4, 1, 0)]
    dt2[!grepl('gras',B_LU_NAME) & p_cat == 'autumn', tcf := fifelse(month == 10,1, 0)]
    
    # add timings for winter cereal
    dt2[grepl('^nl_',B_LU) & grepl('tarwe|wheat',B_LU_NAME) & grepl('winter|wheat',B_LU_NAME), tcf := fifelse(month == 9,1,0)]
    
    # all other crops, assume amendment month is April
    dt2[is.na(tcf), tcf := fifelse(month == 3,1,0)]
    
  } else {
    
    # make a copy if month is already given
    dt2 <- copy(dt)[,tcf := 1]
    
    # when month is unknown, assume that month is April
    dt2[is.na(month),month := 4]
    
  }
  
  # sum all inputs per crop and year
  cols <- c('cin_hum','cin_dpm','cin_rpm')
  dt2 <- dt2[,lapply(.SD,function(x) sum(x * tcf)),
             .SDcols = cols,
             by = c('B_LU','B_LU_NAME','year','month')]
  
  # setorder datatable
  setorder(dt2,year,month)
  
  # add cumulative time vector
  dt2[,time := year + month / 12 - min(year)]
  
  # select only those events that manure input occurs
  dt2 <- dt2[cin_hum > 0 | cin_rpm > 0 | cin_dpm > 0]
  
  # add one row to ensure that there is always an input
  if(nrow(dt2) == 0){dt2 <- data.table(time = 1, cin_dpm = 0, cin_rpm = 0, cin_hum = 0.01)}
  
  # select only relevant columns, rename them
  out <- dt2[,list(CDPM = cin_dpm,CRPM = cin_rpm,CHUM = cin_hum,time = time)]
  
  # melt the output table
  out <- melt(out,id.vars = "time", variable.name = "var")
  
  # add method how RothC should treat the event
  out[, method := 'add']
  
  # return output
  return(out)
}
