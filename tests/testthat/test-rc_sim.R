# Test file for rc_sim 
# Testing framework: testthat

test_that("rc_sim correctly checks input validity", {
  soil_properties <- create_soil_properties()
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3

  M_TILLAGE_SYSTEM = 'CT'
  
  rothc_rotation <- create_rotation()
    
 
  rothc_amendment <- create_amendment()
  

      
  weather <- create_weather()
  
  parms <- create_parms()

  # All correct
 expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                   rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                   weather = weather, rothc_parms = parms))
  
  # No amendment table (allowed)
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = NULL, 
                         weather = weather, rothc_parms = parms))
  
    # No crop table (not allowed)
  expect_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH,  M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                   rothc_rotation = NULL, rothc_amendment = rothc_amendment, 
                   weather = weather))
  
  # Simulation longer than rotation and amendment tables
  #parms1 <- parms[, end_date := "2030-10-01"]
  #expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
  #                       B_DEPTH = B_DEPTH, cf_yield = cf_yield, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
  #                       rothc_rotation = rothc_rotation, rothc_amendment = NULL, 
  #                       weather = weather))
})

