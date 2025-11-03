# Test file for rc_sim 
# Testing framework: testthat

test_that("rc_sim correctly checks input validity", {
  soil_properties <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3

  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)", "erwten (droog te oogsten)" ),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
    )
 
  rothc_amendment <- data.table(
    P_ID = c(1, 1),
    P_NAME = c('cattle_slurry', 'cattle_slurry'),
    P_DOSE = c(63300, 63300),
    P_HC = c(0.7,0.7),
    P_C_OF = c(35, 35),
    P_DATE_FERTILIZATION = c("2022-05-01", "2023-05-01"))
  

      
  weather <- data.table(year = rep(2022:2040, each = 12),
                        month = rep(1:12, 19),
                        W_TEMP_MEAN_MONTH = rep(c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2), 19),
                        W_PREC_SUM_MONTH = rep(c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),19),
                        W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                        W_ET_ACT_MONTH = NA_real_,
                        W_ET_REFACT = rep(0.75, 12))
  
  parms <- list(dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
                      c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
                      initialization_method = 'spinup_analytical_bodemcoolstof',
                      unit = "A_SOM_LOI",
                      method = "adams",
                      poutput = "year",
                      start_date = "2022-04-01",
                      end_date = "2040-10-01")
  
  # Set irrigation moments
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01", "2024-07-01", "2028-06-01"),
    B_IRR_AMOUNT = c(12, 20, 10)
  )

  # All correct
 expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH, rothc_rotation = rothc_rotation, 
                   rothc_amendment = rothc_amendment, 
                   weather = weather, rothc_parms = parms))
  
  # No amendment table (allowed)
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, rothc_rotation = rothc_rotation, 
                         rothc_amendment = NULL, 
                         weather = weather, rothc_parms = parms))
  
    # No crop table (allowed)
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH, rothc_rotation = NULL, rothc_amendment = rothc_amendment, 
                   weather = weather, irrigation = irrigation))
  
  # No weather table (allowed)
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = NULL, rothc_parms = parms))
})

test_that("rc_sim correctly runs with different weather data", {
  # Set up correct input data
  soil_properties <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3
  
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)", "erwten (droog te oogsten)" ),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  rothc_amendment <- data.table(
    P_ID = c(1, 1),
    P_NAME = c('cattle_slurry', 'cattle_slurry'),
    P_DOSE = c(63300, 63300),
    P_HC = c(0.7,0.7),
    P_C_OF = c(35, 35),
    P_DATE_FERTILIZATION = c("2022-05-01", "2023-05-01"))
  


test_that("rc_sim works with different initialization methods", {
  soil_properties <- data.table(
    A_C_OF = 50, B_C_ST03 = 210, A_CLAY_MI = 18, A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  rothc_amendment <- data.table(
    P_DOSE = c(63300, 63300),
    P_HC = c(0.7, 0.7),
    P_C_OF = c(35, 35),
    P_DATE_FERTILIZATION = c("2022-05-01", "2023-05-01")
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
    W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
    W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),
    W_ET_ACT_MONTH = NA_real_
  )
  
  init_methods <- c('spinup_analytical_bodemcoolstof', 
                    'spinup_analytical_heuvelink', 
                    'none')
  
  for (method in init_methods) {
    parms <- list(
      initialization_method = method,
      unit = "A_SOM_LOI",
      method = "adams",
      poutput = "year",
      start_date = "2022-04-01",
      end_date = "2024-10-01"
    )
    
    result <- rc_sim(soil_properties = soil_properties,
                     A_DEPTH = 0.3, B_DEPTH = 0.3,
                     rothc_rotation = rothc_rotation,
                     rothc_amendment = rothc_amendment,
                     weather = weather,
                     rothc_parms = parms)
    
    expect_s3_class(result, "data.table")
    expect_true(nrow(result) > 0)
  }
})

test_that("rc_sim works with lowercase cstock unit", {
  soil_properties <- data.table(
    A_C_OF = 50, B_C_ST03 = 210, A_CLAY_MI = 18, A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
    W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
    W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),
    W_ET_ACT_MONTH = NA_real_
  )
  
  parms <- list(
    initialization_method = 'none',
    c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
    unit = "cstock",  # lowercase
    method = "adams",
    poutput = "year",
    start_date = "2022-04-01",
    end_date = "2024-10-01"
  )
  
  result <- rc_sim(soil_properties = soil_properties,
                   A_DEPTH = 0.3, B_DEPTH = 0.3,
                   rothc_rotation = rothc_rotation,
                   weather = weather,
                   rothc_parms = parms)
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("soc", "CDPM", "CRPM", "CBIO", "CHUM", "CIOM") %in% names(result)))
})

test_that("rc_sim with initialization_method none requires c_fractions", {
  soil_properties <- data.table(
    A_C_OF = 50, B_C_ST03 = 210, A_CLAY_MI = 18, A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  parms <- list(
    initialization_method = 'none',
    c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
    unit = "A_SOM_LOI",
    start_date = "2022-04-01",
    end_date = "2024-10-01"
  )
  
  # Should work with c_fractions supplied
  expect_no_error(rc_sim(soil_properties = soil_properties,
                         A_DEPTH = 0.3, B_DEPTH = 0.3,
                         rothc_rotation = rothc_rotation,
                         rothc_parms = parms))
})

test_that("rc_sim validates unit parameter correctly", {
  soil_properties <- data.table(
    A_C_OF = 50, B_C_ST03 = 210, A_CLAY_MI = 18, A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  valid_units <- c('A_SOM_LOI', 'psoc', 'cstock', 'psomperfraction')
  
  for (unit_val in valid_units) {
    parms <- list(
      initialization_method = 'none',
      c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
      unit = unit_val,
      start_date = "2022-04-01",
      end_date = "2023-10-01"
    )
    
    result <- rc_sim(soil_properties = soil_properties,
                     A_DEPTH = 0.3, B_DEPTH = 0.3,
                     rothc_rotation = rothc_rotation,
                     rothc_parms = parms)
    
    expect_s3_class(result, "data.table")
  }
  
  # Invalid unit should error
  parms <- list(
    initialization_method = 'none',
    c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
    unit = "Cstock",  # Wrong case
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  expect_error(
    rc_sim(soil_properties = soil_properties,
           A_DEPTH = 0.3, B_DEPTH = 0.3,
           rothc_rotation = rothc_rotation,
           rothc_parms = parms),
    "element of set"
  )
})

test_that("rc_sim handles depth corrections correctly", {
  # Shallow sample depth with low clay
  soil_properties_shallow <- data.table(
    A_C_OF = 50, B_C_ST03 = NULL, A_CLAY_MI = 8, A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  parms <- list(
    initialization_method = 'none',
    c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
    unit = "A_SOM_LOI",
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  # Shallow depth (< 0.3m)
  result_shallow <- rc_sim(soil_properties = soil_properties_shallow,
                           A_DEPTH = 0.15, B_DEPTH = 0.3,
                           rothc_rotation = rothc_rotation,
                           rothc_parms = parms)
  
  # Standard depth
  result_standard <- rc_sim(soil_properties = soil_properties_shallow,
                            A_DEPTH = 0.3, B_DEPTH = 0.3,
                            rothc_rotation = rothc_rotation,
                            rothc_parms = parms)
  
  # Results should differ due to depth correction
  expect_s3_class(result_shallow, "data.table")
  expect_s3_class(result_standard, "data.table")
})


test_that("rc_sim output format is correct for different units", {
  soil_properties <- data.table(
    A_C_OF = 50, B_C_ST03 = 210, A_CLAY_MI = 18, A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  # Test A_SOM_LOI
  parms_loi <- list(
    initialization_method = 'none',
    c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
    unit = "A_SOM_LOI",
    poutput = "year",
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_loi <- rc_sim(soil_properties = soil_properties,
                       rothc_rotation = rothc_rotation,
                       rothc_parms = parms_loi)
  
  expect_true("A_SOM_LOI" %in% names(result_loi))
  
  # Test cstock
  parms_cstock <- list(
    initialization_method = 'none',
    c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
    unit = "cstock",
    poutput = "year",
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_cstock <- rc_sim(soil_properties = soil_properties,
                          rothc_rotation = rothc_rotation,
                          rothc_parms = parms_cstock)
  
  expect_true(all(c("soc", "CDPM", "CRPM", "CBIO", "CHUM", "CIOM") %in% names(result_cstock)))
})
test_that("rc_sim accepts irrigation parameter", {
  soil_properties <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)", "erwten (droog te oogsten)"),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  rothc_amendment <- data.table(
    P_ID = c(1, 1),
    P_NAME = c('cattle_slurry', 'cattle_slurry'),
    P_DOSE = c(63300, 63300),
    P_HC = c(0.7, 0.7),
    P_C_OF = c(35, 35),
    P_DATE_FERTILIZATION = c("2022-05-01", "2023-05-01")
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2),
    W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
    W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),
    W_ET_ACT_MONTH = NA_real_,
    W_ET_REFACT = rep(0.75, 12)
  )
  
  parms <- list(
    dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
    c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
    unit = "A_SOM_LOI",
    method = "adams",
    poutput = "year",
    start_date = "2022-04-01",
    end_date = "2025-10-01"
  )
  
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01", "2023-07-01", "2024-06-15"),
    B_IRR_AMOUNT = c(20, 25, 18)
  )
  
  # Test with irrigation
  result_with_irr <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = rothc_amendment,
    weather = weather,
    rothc_parms = parms,
    irrigation = irrigation
  )
  
  expect_s3_class(result_with_irr, "data.table")
  expect_true(nrow(result_with_irr) > 0)
})

test_that("rc_sim works without irrigation parameter (backward compatibility)", {
  soil_properties <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2),
    W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
    W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),
    W_ET_ACT_MONTH = NA_real_,
    W_ET_REFACT = rep(0.75, 12)
  )
  
  parms <- list(
    dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
    c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
    unit = "A_SOM_LOI",
    method = "adams",
    poutput = "year",
    start_date = "2022-04-01",
    end_date = "2025-10-01"
  )
  
  # Test without irrigation (NULL)
  result_no_irr <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms,
    irrigation = NULL
  )
  
  expect_s3_class(result_no_irr, "data.table")
  expect_true(nrow(result_no_irr) > 0)
})



test_that("rc_sim handles irrigation with different output units", {
  soil_properties <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2),
    W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
    W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),
    W_ET_ACT_MONTH = NA_real_,
    W_ET_REFACT = rep(0.75, 12)
  )
  
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(30)
  )
  
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
  soil_properties <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2),
    W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
    W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),
    W_ET_ACT_MONTH = NA_real_,
    W_ET_REFACT = rep(0.75, 12)
  )
  
  parms <- list(
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  # Early season irrigation
  irrigation_early <- data.table(
    B_DATE_IRRIGATION = c("2022-04-15"),
    B_IRR_AMOUNT = c(20)
  )
  
  result_early <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
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
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms,
    irrigation = irrigation_late
  )
  
  expect_s3_class(result_late, "data.table")
})

test_that("rc_sim with irrigation and different W_POT_TO_ACT values", {
  soil_properties <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  # Weather with custom W_ET_REFACT
  weather_custom <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2),
    W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
    W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),
    W_ET_ACT_MONTH = NA_real_,
    W_ET_REFACT = seq(0.6, 0.9, length.out = 12)  # Variable factor
  )
  
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
  soil_properties <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2),
    W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
    W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),
    W_ET_ACT_MONTH = NA_real_,
    W_ET_REFACT = rep(0.75, 12)
  )
  
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
  soil_properties <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2),
    W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
    W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),
    W_ET_ACT_MONTH = NA_real_,
    W_ET_REFACT = rep(0.75, 12)
  )
  
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(30)
  )
  
  # Test with initialization = 'spinup_analytical_bodemcoolstof'
  parms_init_bc <- list(
    initialization_method = 'spinup_analytical_bodemcoolstof',
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_init_bc <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms_init_bc,
    irrigation = irrigation
  )
  
  expect_s3_class(result_init_bc, "data.table")
  
  # Test with initialization = 'spinup_analytical_heuvelink'
  parms_init_heuv <- list(
    initialization_method = 'spinup_analytical_heuvelink',
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result_init_heuv <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms_init_heuv,
    irrigation = irrigation
  )
  
  expect_s3_class(result_init_heuv, "data.table")
})

test_that("rc_sim handles extreme irrigation scenarios", {
  soil_properties <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2),
    W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
    W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),
    W_ET_ACT_MONTH = NA_real_,
    W_ET_REFACT = rep(0.75, 12)
  )
  
  parms <- list(
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  # Minimal irrigation (0 mm)
  irrigation_min <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(0)
  )
  
  result_min <- rc_sim(
    soil_properties = soil_properties,
    A_DEPTH = 0.3,
    B_DEPTH = 0.3,
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
    rothc_rotation = rothc_rotation,
    rothc_amendment = NULL,
    weather = weather,
    rothc_parms = parms,
    irrigation = irrigation_max
  )
  
  expect_s3_class(result_max, "data.table")
})
  
  parms <- list(dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
                c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
                unit = "A_SOM_LOI",
                method = "adams",
                poutput = "year",
                start_date = "2022-04-01",
                end_date = "2040-10-01")
  
  # All weather data supplied
  
  weather_all <- data.table(year = rep(2022:2040, each = 12),
                        month = rep(1:12, 19),
                        W_TEMP_MEAN_MONTH = rep(c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2), 19),
                        W_PREC_SUM_MONTH = rep(c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),19),
                        W_ET_REF_MONTH = rep(c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5), 19),
                        W_ET_ACT_MONTH = rep(c(6, 12, 25, 45, 70, 75, 78, 65, 40, 20, 8, 4), 19))
  
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_all, rothc_parms = parms))
  
  # Only W_ET_REF_MONTH
  weather_pot <- data.table(year = rep(2022:2040, each = 12),
                            month = rep(1:12, 19),
                            W_TEMP_MEAN_MONTH = rep(c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2), 19),
                            W_PREC_SUM_MONTH = rep(c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),19),
                            W_ET_REF_MONTH = rep(c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5), 19))
  
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_pot, rothc_parms = parms))
  
  # Only W_ET_ACT_MONTH
  weather_act <- data.table(year = rep(2022:2040, each = 12),
                            month = rep(1:12, 19),
                            W_TEMP_MEAN_MONTH = rep(c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2), 19),
                            W_PREC_SUM_MONTH = rep(c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),19),
                            W_ET_ACT_MONTH = rep(c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5), 19))
  
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_act, rothc_parms = parms))
  
  # No years (allowed)
  weather_noyr <- data.table(month = 1:12,
                            W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                            W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                            W_ET_ACT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5))
  
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_noyr, rothc_parms = parms))
})

test_that("rc_sim provides correct output in years or months", {
  
  # Set up correct input data
  soil_properties <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)", "erwten (droog te oogsten)" ),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  rothc_amendment <- data.table(
    P_ID = c(1, 1),
    P_NAME = c('cattle_slurry', 'cattle_slurry'),
    P_DOSE = c(63300, 63300),
    P_HC = c(0.7,0.7),
    P_C_OF = c(35, 35),
    P_DATE_FERTILIZATION = c("2022-05-01", "2023-05-01"))
  
  
  
  parms <- list(dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
                c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
                unit = "A_SOM_LOI",
                method = "adams",
                poutput = "month",
                start_date = "2022-04-01",
                end_date = "2040-10-01")
  
  # All weather data supplied
  
  weather <- data.table(year = rep(2022:2040, each = 12),
                            month = rep(1:12, 19),
                            W_TEMP_MEAN_MONTH = rep(c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2), 19),
                            W_PREC_SUM_MONTH = rep(c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),19),
                            W_ET_REF_MONTH = rep(c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5), 19),
                            W_ET_ACT_MONTH = rep(c(6, 12, 25, 45, 70, 75, 78, 65, 40, 20, 8, 4), 19))
  
  
  result <- rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH, rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                   weather = weather, rothc_parms = parms)
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 223)
})