# Test file for rc_sim 
# Testing framework: testthat

testthat::source_file("helper-testdata.R")

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
  
  # No weather table (allowed)
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = NULL, rothc_parms = parms))
})

test_that("rc_sim correctly runs with different weather data", {
  # Set up correct input data
  soil_properties <- create_soil_properties()
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3
  
  
  M_TILLAGE_SYSTEM = 'CT'
  
  rothc_rotation <- create_rotation()
  
  
  rothc_amendment <- create_amendment()
  
  parms <- create_parms()
  
  # Generate weather data
  weather_all <- create_weather()[rep(1:.N, 19)][, year := rep(2022:2040, each = 12)]
  
  
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_all, rothc_parms = parms))
  
  # Only W_ET_REF_MONTH
  weather_pot <- copy(weather_all)[, W_ET_ACT_MONTH := NULL]
  
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_pot, rothc_parms = parms))
  
  # Only W_ET_ACT_MONTH
  weather_act <- copy(weather_all)[, W_ET_REF_MONTH := NULL]
  weather_act[, W_ET_ACT_MONTH := rep(c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5), 19)]
  
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_act, rothc_parms = parms))
  
  # No years (allowed)
  weather_noyr <- create_weather()
  
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_noyr, rothc_parms = parms))
})

test_that("rc_sim provides correct output in years or months", {
  
  # Set up correct input data
  soil_properties <- create_soil_properties()
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3
  
  
  M_TILLAGE_SYSTEM = 'CT'
  
  rothc_rotation <- create_rotation()
  
  rothc_amendment <- create_amendment()
  
  parms <- create_parms()
  parms$poutput <- 'month'
  
  # Weather data
  
  weather <- create_weather()[rep(1:.N, 19)][, year := rep(2022:2040, each = 12)]
  
  
  result <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                   rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                   weather = weather, rothc_parms = parms)
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 223)
})