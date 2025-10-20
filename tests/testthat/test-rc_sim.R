# Test file for rc_sim 
# Testing framework: testthat

test_that("rc_sim correctly runs with valid input", {
  soil_properties <- list(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3

  
  M_TILLAGE_SYSTEM = 'CT'
  
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
  

      
  weather <- data.table(month = 1:12,
                        W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                        W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                        W_ET_POT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                        W_ET_ACT_MONTH = NA_real_)
  
  parms <- list(dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
                      c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
                      initialize = TRUE,
                      unit = "A_SOM_LOI",
                      method = "adams",
                      poutput = "year",
                      start_date = "2022-04-01",
                      end_date = "2040-10-01")

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

test_that("rc_sim correctly returns different output formats", {
  # Set correct  input files
  soil_properties <- list(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3
  
  
  M_TILLAGE_SYSTEM = 'CT'
  
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
  
  
  
  weather <- data.table(month = 1:12,
                        W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                        W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                        W_ET_POT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                        W_ET_ACT_MONTH = NA_real_)
  
  parms <- list(dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
                c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
                initialize = TRUE,
                method = "adams",
                poutput = "year",
                start_date = "2022-04-01",
                end_date = "2040-10-01")
  
  
  # unit = "A_SOM_LOI
  parms_somloi <- parms
  
  parms_somloi$unit <- "A_SOM_LOI"
  
  result_somloi <- rc_sim(
    soil_properties = soil_properties,
    rothc_rotation = rothc_rotation,
    rothc_amendment = rothc_amendment,
    weather = weather,
    rothc_parms = parms_somloi
  )

  expect_s3_class(result_somloi, "data.table")
  expect_true("A_SOM_LOI" %in% names(result_somloi))
  expect_true("year" %in% names(result_somloi))
  expect_false(any(is.na(result_somloi$A_SOM_LOI)))
  
  # unit = "psoc"
  parms_psoc <- parms
  
  parms_psoc$unit <- "psoc"
  
  result_psoc <- rc_sim(
    soil_properties = soil_properties,
    rothc_rotation = rothc_rotation,
    rothc_amendment = rothc_amendment,
    weather = weather,
    rothc_parms = parms_psoc
  )
  
  expect_s3_class(result_psoc, "data.table")
  expect_true("psoc" %in% names(result_psoc))
  expect_true("year" %in% names(result_psoc))
  expect_false(any(is.na(result_psoc$psoc)))
  
  # unit = "psomperfraction"
  parms_pspf <- parms
  
  parms_pspf$unit <- "psomperfraction"
  
  result_pspf <- rc_sim(
    soil_properties = soil_properties,
    rothc_rotation = rothc_rotation,
    rothc_amendment = rothc_amendment,
    weather = weather,
    rothc_parms = parms_pspf
  )
  
  expect_s3_class(result_pspf, "data.table")
  expect_true(all(c("CDPM", "CRPM", "CBIO", "CHUM", "CIOM") %in% names(result_pspf)))
  expect_true("year" %in% names(result_pspf))
  expect_false(any(is.na(result_pspf$CDPM)))
  
  
  # unit = "cstock"
  parms_cstock <- parms
  
  parms_cstock$unit <- "cstock"
  
  result_cstock <- rc_sim(
    soil_properties = soil_properties,
    rothc_rotation = rothc_rotation,
    rothc_amendment = rothc_amendment,
    weather = weather,
    rothc_parms = parms_cstock
  )
  
  expect_s3_class(result_cstock, "data.table")
  expect_true(all(c("soc", "CDPM", "CRPM", "CBIO", "CHUM", "CIOM") %in% names(result_cstock)))
  expect_true("year" %in% names(result_cstock))
  expect_false(any(is.na(result_cstock$soc)))
  
  # invalid unit (should error)
  parms_invalid <- parms
  
  parms_invalid$unit <- "invalid_unit"
  
  expect_error(rc_sim(
    soil_properties = soil_properties,
    rothc_rotation = rothc_rotation,
    rothc_amendment = rothc_amendment,
    weather = weather,
    rothc_parms = parms_invalid
  ), "additional elements")
}
)

test_that("rc_sim returns yearly output when poutput is 'year'", {
  soil_properties <- list(
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
    W_ET_POT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),
    W_ET_ACT_MONTH = NA_real_
  )
  
  parms <- list(
    dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
    c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
    initialize = TRUE,
    unit = "A_SOM_LOI",
    method = "adams",
    poutput = "year",
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  result <- rc_sim(
    soil_properties = soil_properties,
    rothc_rotation = rothc_rotation,
    rothc_amendment = rothc_amendment,
    weather = weather,
    rothc_parms = parms
  )
  
  expect_true(all(result$time %% 1 == 0))
})

test_that("rc_sim runs in debug mode and produces debug output", {
  soil_properties <- list(
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
    W_ET_POT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),
    W_ET_ACT_MONTH = NA_real_
  )
  
  parms <- list(
    dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
    c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
    initialize = TRUE,
    unit = "A_SOM_LOI",
    method = "adams",
    poutput = "year",
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  # Remove debug files if they exist
  if (file.exists("rothc_c_flows_debug.csv")) file.remove("rothc_flows_debug.csv")
  if (file.exists("carbon_pools_log.png")) file.remove("carbon_pools_log.png")
  if (file.exists("carbon_pools_linear.png")) file.remove("carbon_pools_linear.png")
  if (file.exists("carbon_pools_change.png")) file.remove("carbon_pools_change.png")
  
  # Run in debug mode
  expect_no_error(
    rc_sim(
      soil_properties = soil_properties,
      rothc_rotation = rothc_rotation,
      rothc_amendment = rothc_amendment,
      weather = weather,
      rothc_parms = parms,
      debug = TRUE
    )
  )
  
  # Check that debug files were created
  expect_true(file.exists("rothc_flows_debug.csv"))
  expect_true(file.exists("carbon_pools_log.png"))
  expect_true(file.exists("carbon_pools_linear.png"))
  expect_true(file.exists("carbon_pools_change.png"))
  
  # Check that debug file has expected columns
  debug_output <- fread("rothc_flows_debug.csv")
  expect_true(all(c("time", "CDPM", "CRPM", "CBIO", "CHUM", "soc") %in% names(debug_output)))
  
  # Clean up
  file.remove("rothc_flows_debug.csv")
  file.remove("carbon_pools_log.png")
  file.remove("carbon_pools_linear.png")
  file.remove("carbon_pools_change.png")
})