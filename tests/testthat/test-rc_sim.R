# Test file for rc_sim 
# Testing framework: testthat

test_that("rc_sim correctly checks input validity", {
  soil_properties <- list(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3
  
  cf_yield = 1
  
  M_TILLAGE_SYSTEM = 'CT'
  
  rothc_rotation <- data.table(
    year = c(2022, 2023),
    B_LU = c("nl_308", "nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)", "erwten (droog te oogsten)" ),
    B_LU_EOM_CROP = c(170, 170),
    B_LU_EOM_CROPRESIDUE = c(830, 830),
    B_LU_HC = c(0.32, 0.32))
 
  rothc_amendment <- data.table(
    year = c(2022, 2023),
    month = c(5, 5),
    P_NAME = c('cattle_slurry', 'cattle_slurry'),
    P_DOSE = c(63300, 63300),
    P_HC = c(0.7,0.7),
    P_OM = c(7.1, 7.1),
    p_p2o5 = c(0.15, 0.15))
      
  weather <- data.table(month = 1:12,
                        W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                        W_PREC_MEAN_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                        W_ET_POT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                        W_ET_ACT_MONTH = NA_real_)

  # All correct
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH, cf_yield = cf_yield, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                   rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                   weather = weather))
  
  # No amendment table (not allowed)
  expect_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, cf_yield = cf_yield, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = NULL, 
                         weather = weather))
  
  # No month in amendment table (allowed)
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                      B_DEPTH = B_DEPTH, cf_yield = cf_yield, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                      rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment[,.SD, .SDcols = !"month"], 
                      weather = weather))
  
  # No crop table (not allowed)
  expect_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH, cf_yield = cf_yield, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                   rothc_rotation = rothc_rotation, rothc_amendment = NULL, 
                   weather = weather))
  

})



