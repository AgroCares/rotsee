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
  
  # add crop names and catch crop characteristics to table
  dt <- merge(dt,cc.crop,by = 'B_LU',all.x = TRUE)
 
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
  
  # select lowest input of catch crop, equal to latest implementation catch crop
  dt.green <- dt.green[, .SD[which.min(green_eom)], by = year]
  
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
  
  # estimate total EOC input
  
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