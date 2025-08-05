#' Function to prepare inputs for various scenarios with RothC (for the Netherlands)
#' 
#' This functions prepares the input for scenarios differing in C inputs.
#' 
#' @param B_LU_BRP (numeric) value of the BRP crop code
#' @param scen (character) possible scenarios. Include BAU, CLT, BAUIMPR and ALL
#' @param cf_yield (numeric) A yield correction factor (fraction) if yield is higher than regional average
#' 
#' @export
rc_input_scenario <- function(B_LU_BRP, scen, cf_yield = 1){
  
  # add visual bindings
  . = B_LU_NAME = B_LU_EOM = B_LU_EOM_RESIDUE = B_LU_HC = B_LU_WATERSTRESS_OBIC = NULL
  B_LU = gld = cereal = nat = bld = M_GREEN_TIMING = M_CROPRESIDUE = man_name = NULL
  P_OM = P_HC = P_p2o5 = P_DOSE = P_NAME = p_p2o5 = crop_code = crop_name = NULL
  
  # check inputs
  checkmate::assert_numeric(cf_yield,lower = 0.1, upper = 2.0, any.missing = FALSE,len = 1)
  checkmate::assert_integerish(B_LU_BRP, any.missing = FALSE, null.ok = TRUE, min.len = 1)
  checkmate::assert_subset(B_LU_BRP, choices = unique(rotsee::rc_crops$crop_code), empty.ok = TRUE)
  
  # composition table for cattle slurry and compost
  dtcm <- data.table(man_name = c('cattle_slurry','green_compost'),
                     P_OM = c(7.1,17.9),
                     P_HC = c(0.7,0.9),
                     p_p2o5 = c(0.15,0.22))
  
  # combine input data
  dt <- data.table(B_LU_BRP = B_LU_BRP,year = 1:length(B_LU_BRP),cf_yield = cf_yield)
  dt <- merge(dt,
              rotsee::rc_crops[,.(B_LU_BRP = crop_code,B_LU_NAME = crop_name,B_LU_EOM,B_LU_EOM_RESIDUE,
                                B_LU_HC,B_LU_WATERSTRESS_OBIC)],
              by = 'B_LU_BRP',
              all.x=TRUE)
  
  dt[, cat := fifelse(grepl('grasland',B_LU_WATERSTRESS_OBIC),1,0)]
  setorder(dt,year)
  
  # what is the main crop use
  if(sum(dt$cat)/nrow(dt) > 0.5){ luse <- 'GLD' }else{ luse <- 'BLD'}
  
  # Scenario BAU
  
  # Set crop rotation
  rotation <- copy(dt)
  rotation <- rotation[,.(year,B_LU_BRP,B_LU_NAME,B_LU_EOM,B_LU_EOM_RESIDUE,B_LU_HC)]
  
  # set categories
  rotation[, gld := fifelse(grepl('gras',B_LU_NAME),1,0)]
  rotation[, cereal := fifelse(grepl('gerst|tarwe|rogge|haver|granen',B_LU_NAME),1,0)]
  rotation[, nat := fifelse(grepl('bomen|struiken|heesters|contain|wijn|definitief|fauna|boomkwe|natuur|boomgr|scheerheg|hakhout|wandelp|landschaps|zandwal|boom|bufferstr|^rand',B_LU_NAME),1,0)]
  rotation[, bld := 1 - gld - cereal - nat]
  
  # set default green manure
  rotation[grepl('mais|aardappel',B_LU_NAME), M_GREEN_TIMING := 'october']
  rotation[is.na(M_GREEN_TIMING), M_GREEN_TIMING := 'never']
  
  # set default crop residue
  rotation[, M_CROPRESIDUE := FALSE]
  
  # Set amendment
  amendment <- dt[,.(year,cat,B_LU_BRP,B_LU_NAME)]
  amendment[, c('P_NAME','month') := list('cattle_slurry',month = 3)]
  amendment <- merge(amendment,dtcm,by.x = 'P_NAME',by.y = 'man_name',all.x = TRUE)
  amendment[cat == 1, P_DOSE := 63300]
  amendment[cat == 0, P_DOSE := 46700]
  amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME), P_DOSE := 0]
  
  # scenario is BAUIMPR
  if(scen=='BAUIMPR'){
    
    # update timing cover crop
    rotation[, M_GREEN_TIMING := NA_character_]
    rotation[grepl('mais|aardappel',B_LU_NAME), M_GREEN_TIMING := 'september']
    rotation[grepl('gras',B_LU_NAME), M_GREEN_TIMING := 'never']
    rotation[is.na(M_GREEN_TIMING), M_GREEN_TIMING := 'october']
    
    # update manure by partly using compost
    amendment2 <- amendment[,.(year,B_LU_BRP,B_LU_NAME,cat,P_NAME,month)]
    amendment2[,c('P_NAME','month') := list('green_compost',month = 8)]
    amendment <- rbind(amendment[,.(year,B_LU_BRP,B_LU_NAME,cat,P_NAME,month)],amendment2)
    amendment <- merge(amendment,dtcm, by.x = 'P_NAME', by.y = 'man_name', all.x = TRUE)
    
    # Determine dosage of slurry and compost based on crop category
    amendment[cat == 1 & grepl('cattle',P_NAME), P_DOSE := round(95 * 0.95*1000*0.1/ p_p2o5)]
    amendment[cat == 0 & grepl('cattle',P_NAME), P_DOSE := round(70 * 0.95*1000*0.1/ p_p2o5)]
    amendment[cat == 1 & grepl('compost',P_NAME), P_DOSE := round(95 * 0.05*1000*2*0.1/ p_p2o5)]
    amendment[cat == 0 & grepl('compost',P_NAME), P_DOSE := round(70 * 0.05*1000*2*0.1/ p_p2o5)]
    
    amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME) & grepl('cattle',P_NAME), P_DOSE := 0]
    amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME) & grepl('compost',P_NAME), P_DOSE := round(70 * 0.3*1000*2 *.1/ p_p2o5)]
    
    
  }
  
  # only adaptation crop cultivation: permanent grassland or cereals, catch crops and crop residue use
  if(scen == 'CLT'){
    
    # adapt crop input
    if(luse == 'BLD'){B_LU_BRP <- rep(233,length(B_LU_BRP))}
    if(luse == 'GLD'){B_LU_BRP <- rep(265,length(B_LU_BRP))}
    
    # Set crop rotation
    rotation <- copy(dt)
    rotation <- rotation[,.(year,B_LU_NAME,B_LU_EOM,B_LU_EOM_RESIDUE,B_LU_HC)]
    
    # set default green manure
    rotation[grepl('mais|aardappel',B_LU_NAME), M_GREEN_TIMING := 'august']
    rotation[is.na(M_GREEN_TIMING), M_GREEN_TIMING := 'never']
    rotation <- rotation[,.(year,B_LU_NAME,B_LU_EOM,B_LU_EOM_RESIDUE,B_LU_HC,M_GREEN_TIMING)]
    
    # set default crop residue
    rotation[, M_CROPRESIDUE := TRUE]
    
    # Set amendment
    amendment <- dt[,.(year,cat,B_LU_BRP,B_LU_NAME)]
    amendment[, c('P_NAME','month') := list('cattle_slurry',month = 3)]
    amendment <- merge(amendment,dtcm,by.x = 'P_NAME',by.y = 'man_name',all.x = TRUE)
    amendment[cat == 1, P_DOSE := 63300]
    amendment[cat == 0, P_DOSE := 46700]
    amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME), P_DOSE := 0]
    
  }
  
  if(scen=='ALL'){
    
    # set maximum cereal to 40% of the rotation
    fr_gras <- rotation[,sum(gld)/.N]
    fr_cereal <- rotation[,sum(cereal)/.N]
    fr_bld <- rotation[,sum(bld)/.N]
    
    # adapt bouwland
    if(luse=='BLD' & fr_cereal < 0.4){
      
      nextra <- max(0,0.4 - fr_cereal)
      
      rot1 <- rotation[gld==0 & cereal == 0][sample(1:.N,ceiling(nextra * .N)),B_LU_BRP := 233]
      rot2 <- rotation[!(gld==0 & cereal == 0)]
      rotation <- rbind(rot1,rot2)
    }
    
    # increase aandeel gras / leeftijd gras
    if(luse=='GLD' & fr_bld > 0.3){
      
      nextra <- max(0,0.5 * fr_bld)
      
      rot1 <- rotation[bld==1][sample(1:.N,ceiling(nextra * .N)),B_LU_BRP := 265]
      rot2 <- rotation[!bld==1]
      rotation <- rbind(rot1,rot2)
    }
    
    # update properties
    rotation <- merge(rotation[,.(year,B_LU_BRP)],
                      rotsee::rc_crops,by.x = 'B_LU_BRP',
                      by.y='crop_code',all.x=TRUE)
    rotation[, gld := fifelse(grepl('gras',B_LU_NAME),1,0)]
    rotation[, cereal := fifelse(grepl('gerst|tarwe|rogge|haver|granen',B_LU_NAME),1,0)]
    rotation[, nat := fifelse(grepl('bomen|struiken|heesters|contain|wijn|definitief|fauna|boomkwe|natuur|boomgr|scheerheg|hakhout|wandelp|landschaps|zandwal|boom|bufferstr|^rand',B_LU_NAME),1,0)]
    rotation[, bld := 1 - gld - cereal - nat]
    
    # update catch crop
    rotation[grepl('mais|aardappel',B_LU_NAME), M_GREEN_TIMING := 'september']
    rotation[grepl('gras',B_LU_NAME), M_GREEN_TIMING := 'never']
    rotation[is.na(M_GREEN_TIMING), M_GREEN_TIMING := 'october']
    
    # update crop residue input
    rotation[gld == 0 | nat == 0,M_CROPRESIDUE := TRUE]
    rotation[is.na(M_CROPRESIDUE), M_CROPRESIDUE := FALSE]
    
    # update manure by partly using compost
    amendment2 <- amendment[,.(year,B_LU_BRP,B_LU_NAME,cat,P_NAME,month)]
    amendment2[,c('P_NAME','month') := list('green_compost',month = 8)]
    amendment <- rbind(amendment[,.(year,B_LU_BRP,B_LU_NAME,cat,P_NAME,month)],amendment2)
    amendment <- merge(amendment,dtcm, by.x = 'P_NAME', by.y = 'man_name', all.x = TRUE)
    
    # Determine dosage of slurry and compost based on crop category
    amendment[cat == 1 & grepl('cattle',P_NAME), P_DOSE := round(95 * 0.95*1000*0.1/ p_p2o5)]
    amendment[cat == 0 & grepl('cattle',P_NAME), P_DOSE := round(70 * 0.95*1000*0.1/ p_p2o5)]
    amendment[cat == 1 & grepl('compost',P_NAME), P_DOSE := round(95 * 0.05*1000*2*0.1/ p_p2o5)]
    amendment[cat == 0 & grepl('compost',P_NAME), P_DOSE := round(70 * 0.05*1000*2*0.1/ p_p2o5)]
    
    amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME) & grepl('cattle',P_NAME), P_DOSE := 0]
    amendment[grepl('uien|peen|witlof|graszaad|bieten, suiker-',B_LU_NAME) & grepl('compost',P_NAME), P_DOSE := round(70 * 0.3*1000*2 *.1/ p_p2o5)]
    
    
  }
  
  # add default situation with no irrigation
  rotation[, M_IRRIGATION := FALSE]
  
  # subset final rotation
  rotation <- rotation[,.(year,B_LU_BRP,B_LU_EOM,B_LU_EOM_RESIDUE,B_LU_HC,
                          M_GREEN_TIMING,M_CROPRESIDUE,M_IRRIGATION, cf_yield)]
  
  # add B_LU for Dutch situation
  rotation[, B_LU := paste0('nl_',B_LU_BRP)]
  
  # Remove unnecessary columns
  amendment[,c('cat','B_LU_BRP','B_LU_NAME') := NULL]
  rotation[,c('B_LU_BRP') := NULL]
  
  out <- list(rotation = rotation, amendment = amendment)
  return(out)
}
