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
  
  irrigation <- create_irrigation()

  # All correct
 result_all <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                   rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                   weather = weather, rothc_parms = parms, irrigation = irrigation)
 
 expect_s3_class(result_all, "data.table")
 expect_true(nrow(result_all) > 0)
  
  # No amendment table (allowed)
  result_no_amend <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = NULL, 
                         weather = weather, rothc_parms = parms, irrigation = irrigation)
                      
  expect_s3_class(result_no_amend, "data.table")
  expect_true(nrow(result_no_amend) > 0)                 
  
    # No crop table (allowed)
  result_no_crop <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH,  M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                   rothc_rotation = NULL, rothc_amendment = rothc_amendment, 
                   weather = weather, irrigation = irrigation)
  
  expect_s3_class(result_no_crop, "data.table")
  expect_true(nrow(result_no_crop) > 0) 
  
  # No weather table (allowed)
  result_no_weath <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = NULL, rothc_parms = parms, irrigation = irrigation)
  
  expect_s3_class(result_no_weath, "data.table")
  expect_true(nrow(result_no_weath) > 0) 
  
  
  # no irrigation (allowed)
  result_no_irri <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = NULL, rothc_parms = parms)
  
  expect_s3_class(result_no_irri, "data.table")
  expect_true(nrow(result_no_irri) > 0) 
  
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
  

test_that("rc_sim handles irrigation with different output units", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  irrigation <- create_irrigation()
  
  # Test with A_SOM_LOI output
  parms_som <- list(
    unit = "A_SOM_LOI",
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_som <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    M_TILLAGE_SYSTEM = 'CT',
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms_som,
    irrigation = irrigation
  )
  
  expect_s3_class(result_som, "data.table")
  expect_true("A_SOM_LOI" %in% names(result_som))
  
  # Test with psoc output
  parms_psoc <- list(
    unit = "psoc",
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_psoc <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    M_TILLAGE_SYSTEM = 'CT',
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms_psoc,
    irrigation = irrigation
  )
  
  expect_s3_class(result_psoc, "data.table")
  expect_true(all(c("soc", "psoc") %in% names(result_psoc)))
  
  # Test with Cstock output
  parms_cstock <- list(
    unit = "cstock",
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_cstock <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    M_TILLAGE_SYSTEM = 'CT',
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms_cstock,
    irrigation = irrigation
  )
  
  expect_s3_class(result_cstock, "data.table")
  expect_true("soc" %in% names(result_cstock))
})


test_that("rc_sim handles irrigation timing variations", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  parms <- create_parms()
  
  # Early season irrigation
  irrigation_early <- data.table(
    B_DATE_IRRIGATION = c("2022-04-15"),
    B_IRR_AMOUNT = c(20)
  )
  
  result_early <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    M_TILLAGE_SYSTEM = 'CT',
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms,
    irrigation = irrigation_early
  )
  
  expect_s3_class(result_early, "data.table")
  
  # Late season irrigation
  irrigation_late <- data.table(
    B_DATE_IRRIGATION = c("2022-09-15"),
    B_IRR_AMOUNT = c(20)
  )
  
  result_late <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    M_TILLAGE_SYSTEM = 'CT',
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms,
    irrigation = irrigation_late
  )
  
  expect_s3_class(result_late, "data.table")
})

test_that("rc_sim with irrigation and different W_ET_REFACT values", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  # Weather with custom W_ET_REFACT
  weather_custom <- create_weather()
  weather_custom[, W_ET_REFACT := seq(0.6, 0.9, length.out = 12)]# Variable factor
  
  
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(30)
  )
  
  parms <- list(
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    M_TILLAGE_SYSTEM = 'CT',
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather_custom,
    rothc_parms = parms,
    irrigation = irrigation
  )
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

test_that("rc_sim handles long-term simulation with irrigation", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  # Irrigation events spread over multiple years
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01", "2023-07-01", "2024-07-01", 
                          "2025-07-01", "2026-07-01"),
    B_IRR_AMOUNT = c(25, 30, 20, 28, 22)
  )
  
  parms <- list(
    start_date = "2022-04-01",
    end_date = "2027-10-01"
  )
  
  result <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    M_TILLAGE_SYSTEM = 'CT',
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms,
    irrigation = irrigation
  )
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  # Should have multiple years of output
  expect_true(max(result$year) - min(result$year) >= 4)
})

test_that("rc_sim handles irrigation with initialization", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(30)
  )
  
  # Test with initialization = TRUE
  parms_init_true <- list(
    initialize = TRUE,
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_init_true <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    M_TILLAGE_SYSTEM = 'CT',
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms_init_true,
    irrigation = irrigation
  )
  
  expect_s3_class(result_init_true, "data.table")
  
  # Test with initialization = FALSE
  parms_init_false <- list(
    initialize = FALSE,
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_init_false <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    M_TILLAGE_SYSTEM = 'CT',
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms_init_false,
    irrigation = irrigation
  )
  
  expect_s3_class(result_init_false, "data.table")
})

test_that("rc_sim handles extreme irrigation scenarios", {
  soil_properties <- create_soil_properties()
  
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  parms <- create_parms()
  
  # Minimal irrigation (0 mm)
  irrigation_min <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(0)
  )
  
  result_min <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    M_TILLAGE_SYSTEM = 'CT',
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms,
    irrigation = irrigation_min
  )
  
  expect_s3_class(result_min, "data.table")
  
  # Maximum irrigation (1000 mm - extreme case)
  irrigation_max <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(1000)
  )
  
  result_max <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    M_TILLAGE_SYSTEM = 'CT',
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms,
    irrigation = irrigation_max
  )
  
  expect_s3_class(result_max, "data.table")
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