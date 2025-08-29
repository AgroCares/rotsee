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
  B_LU = B_LU_NAME = p_cat = fr_eoc_p = crflt = tcf = NULL
  cin_hum = cin_rpm = cin_dpm = method = crop_code = crop_name = NULL
  fr_eoc_p = time = NULL
  
  # make local copy
  dt <- copy(amendment)
  
  # make default crop amendment data.table when dt = NULL
  if(is.null(dt)){dt <- data.table(year = crops[1,year], month = 1, cin_tot = 0, cin_hum = 0,
                                   cin_dpm = 0, cin_rpm = 0, fr_eoc_p = 10)}
  
  # do checks on the crop list
  checkmate::assert_data_table(crops)
  checkmate::assert_true('B_LU' %in% colnames(crops))
  checkmate::assert_true('year' %in% colnames(crops))
  
  # do checks on the input of C due to organic amendments
  checkmate::assert_data_table(dt)
  checkmate::assert_names(names(dt),
    must.include = c('year','cin_hum','cin_dpm','cin_rpm','fr_eoc_p'),
    subset.of    = c('year','month','p_name','cin_tot','cin_hum','cin_dpm','cin_rpm','fr_eoc_p'))
  checkmate::assert_numeric(dt$cin_hum,lower = 0, upper = 100000,len = nrow(dt))
  if("cin_tot" %in% names(dt)){
  checkmate::assert_numeric(dt$cin_tot,lower = 0, upper = 100000,len = nrow(dt))}
  checkmate::assert_numeric(dt$cin_dpm,lower = 0, upper = 100000,len = nrow(dt))
  checkmate::assert_numeric(dt$cin_rpm,lower = 0, upper = 100000,len = nrow(dt))
  checkmate::assert_numeric(dt$fr_eoc_p,lower = 0, upper = 250,len = nrow(dt))
  checkmate::assert_integerish(dt$year,len = nrow(dt))
  
  # load internal tables
  cc.crop <- as.data.table(rotsee::rc_crops)
  cc.crop[,B_LU := paste0('nl_',crop_code)]
  
  # merge table with amendment c input with crop name
  dt <- merge(dt, crops[,list(B_LU,year)], by='year', all.x = TRUE)
  dt <- merge(dt, cc.crop[,list(B_LU,B_LU_NAME=crop_name)],by='B_LU', all.x = TRUE)
  
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
    dt2[
      grepl('^nl_', B_LU) &
      grepl('tarwe|wheat', B_LU_NAME, ignore.case = TRUE) &
      grepl('winter',   B_LU_NAME, ignore.case = TRUE),
      tcf := fifelse(month == 9, 1, 0)
    ]
    # all other crops, assume amendment month is April
    dt2[is.na(tcf), tcf := fifelse(month == 4, 1, 0)]
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