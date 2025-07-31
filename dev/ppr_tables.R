# load packages
require(data.table);library(usethis)

# make crop table
  
  # load table with crop properties from pandex (private repo)
  rc_crops <- pandex::b_lu_brp[,.(crop_code = B_LU_BRP,
                                  crop_name = B_LU_NAME,
                                  crop_cat1 = B_LU_CULTCAT4,
                                  country ="NL",
                                  B_LU = B_LU,
                                  B_LU_EOM,
                                  B_LU_EOM_RESIDUE,
                                  B_LU_HC,
                                  B_LU_WATERSTRESS_OBIC,
                                  B_LU_MAKKINK)]
  
  # replace löss with loess
  rc_crops[grepl('löss',crop_name), crop_name := gsub('löss','loess',crop_name)]
  
  # switch to english categories
  rc_crops[crop_cat1=='akkerbouw', crop_cat1 := 'arable']
  rc_crops[crop_cat1=='mais', crop_cat1 := 'maize']
  rc_crops[crop_cat1=='grasland', crop_cat1 := 'grassland']
  rc_crops[crop_cat1=='natuur', crop_cat1 := 'nature']
  
  # save updated crop table
  usethis::use_data(rc_crops,overwrite = TRUE)

# make makkink table

  # load csv file with makkink per crop per month
  dt <- fread('data-raw/rc_makkink.csv',header = TRUE)
  
  # colnames for the 12 months
  cols <- colnames(dt)[-1]
  
  # replace all missing values with the value 0.36 for braak soil
  dt[,c(cols) := lapply(.SD,function(x) fifelse(is.na(x),0.36,x)),.SDcols = cols]
  
  # write file to data
  rc_makkink <- copy(dt)
  
  # write file to data
  use_data(rc_makkink, overwrite = TRUE)
  