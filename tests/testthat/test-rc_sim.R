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
                      initialisation_method = 'spinup_analytical_bodemcoolstof',
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
                   B_DEPTH = B_DEPTH, 
                   rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                   weather = weather, rothc_parms = parms, irrigation = irrigation))
  
  # No amendment table (allowed)
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, 
                         rothc_rotation = rothc_rotation, rothc_amendment = NULL, 
                         weather = weather, rothc_parms = parms, irrigation = irrigation))
  
    # No crop table (allowed)
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH,  
                   rothc_rotation = NULL, rothc_amendment = rothc_amendment, 
                   weather = weather, irrigation = irrigation))
  
  # No weather table (allowed)
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, 
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = NULL, rothc_parms = parms))
})


  
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


test_that("rc_sim correctly returns different output formats", {
  # Set correct  input files
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
  
  
  
  weather <- data.table(month = 1:12,
                        W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                        W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                        W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                        W_ET_ACT_MONTH = NA_real_)
  
  parms <- list(dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
                c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
                initialisation_method = 'spinup_simulation',
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
  expect_true("soc" %in% names(result_psoc))
  expect_true("year" %in% names(result_psoc))
  expect_false(any(is.na(result_psoc$soc)))
  
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
  ), "Must be element of set")
}
)

test_that("rc_sim returns yearly output when poutput is 'year'", {
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
    W_ET_ACT_MONTH = NA_real_
  )
  
  parms <- list(
    dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
    c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
    initialisation_method = 'spinup_simulation',
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
  # Verify yearly output: should have one row per year
  expect_equal(nrow(result), 2)  # 2022-04-01 to 2023-10-01 spans 2 years
  expect_true("year" %in% names(result))
  expect_equal(result$year, c(2022, 2023))
})

test_that("rc_sim runs in visualize mode and produces visualize output", {
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
    W_ET_ACT_MONTH = NA_real_
  )
  
  parms <- list(
    dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
    c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
    initialisation_method = 'spinup_simulation',
    unit = "A_SOM_LOI",
    method = "adams",
    poutput = "year",
    start_date = "2022-04-01",
    end_date = "2023-10-01"
  )
  
  # Save current working directory and switch to temp directory
  old_wd <- getwd()
  temp_dir <- file.path(tempdir(), "rotsee_debug_test")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  setwd(temp_dir)
  
  # Restore working directory on exit and remove temporary directory(even if test fails)
  on.exit({
    setwd(old_wd)
    unlink(temp_dir, recursive = TRUE)
  }, add = TRUE)
  

  # Run in debug mode
  expect_no_error(
    rc_sim(
      soil_properties = soil_properties,
      rothc_rotation = rothc_rotation,
      rothc_amendment = rothc_amendment,
      weather = weather,
      rothc_parms = parms,
      visualize = TRUE
    )
  )
 
  # Check that debug files were created
  expect_true(file.exists("rothc_flows_vis.csv"))
  expect_true(file.exists("carbon_pools_linear.png"))
  expect_true(file.exists("carbon_pools_change.png"))
  
  # Check that debug file has expected columns
  vis_output <- fread("rothc_flows_vis.csv")
  expect_true(all(c("time", "CDPM", "CRPM", "CBIO", "CHUM", "soc") %in% names(vis_output)))

})

test_that("rc_sim works with different initialisation methods", {
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
                    'spinup_simulation',
                    'none')
  
  for (method in init_methods) {
    parms <- list(
      initialisation_method = method,
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


test_that("rc_sim with initialisation_method none correctly uses c_fractions", {
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
    initialisation_method = 'none',
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
  
  # Should work without c_fractions supplied
  parms_no_fractions <- list(
    initialisation_method = 'none',
    unit = "A_SOM_LOI",
    start_date = "2022-04-01",
    end_date = "2024-10-01"
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
  expect_no_error(rc_sim(soil_properties = soil_properties,
                      A_DEPTH = 0.3, B_DEPTH = 0.3,
                      rothc_rotation = rothc_rotation,
                      rothc_parms = parms_no_fractions))
  
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
    initialisation_method = 'none',
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
  expect_false(isTRUE(all.equal(result_shallow$A_SOM_LOI[nrow(result_shallow)], 
                                result_standard$A_SOM_LOI[nrow(result_standard)])))
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

test_that("rc_sim with irrigation and different W_ET_REFACT values", {
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

test_that("rc_sim handles irrigation with initialisation", {
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
  
  # Test with initialisation = 'spinup_analytical_bodemcoolstof'
  parms_init_bc <- list(
    initialisation_method = 'spinup_analytical_bodemcoolstof',
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
  
  # Test with initialisation = 'spinup_analytical_heuvelink'
  parms_init_heuv <- list(
    initialisation_method = 'spinup_analytical_heuvelink',
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
  
  parms <- list(dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
                c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
                initialize = TRUE,
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
                         B_DEPTH = B_DEPTH, 
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_all, rothc_parms = parms))
  
  # Only W_ET_REF_MONTH
  weather_pot <- data.table(year = rep(2022:2040, each = 12),
                            month = rep(1:12, 19),
                            W_TEMP_MEAN_MONTH = rep(c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2), 19),
                            W_PREC_SUM_MONTH = rep(c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),19),
                            W_ET_REF_MONTH = rep(c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5), 19))
  
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, 
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_pot, rothc_parms = parms))
  
  # Only W_ET_ACT_MONTH
  weather_act <- data.table(year = rep(2022:2040, each = 12),
                            month = rep(1:12, 19),
                            W_TEMP_MEAN_MONTH = rep(c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2), 19),
                            W_PREC_SUM_MONTH = rep(c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),19),
                            W_ET_ACT_MONTH = rep(c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5), 19))
  
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, 
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                         weather = weather_act, rothc_parms = parms))
  
  # No years (allowed)
  weather_noyr <- data.table(month = 1:12,
                            W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                            W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                            W_ET_ACT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5))
  
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, 
                         rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
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
                initialize = TRUE,
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
                   B_DEPTH = B_DEPTH, 
                   rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                   weather = weather, rothc_parms = parms)
  
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 223)
})

test_that("rc_sim works with limited data input", {
  # define soil properties
  soil <- data.table(
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  # define parms to set simulation period
  parms <- list(
    start_date = "2022-01-01",
    end_date = "2032-01-01"
  )
  
  # Run the model
  results <- rc_sim(
    soil_properties = soil,
    rothc_parms = parms
  )
  
  expect_s3_class(results, "data.table")
  dt.time <- rc_time_period(parms$start_date, parms$end_date)
  expect_equal(nrow(results), nrow(dt.time))
  
})

test_that("rc_sim handles start_date falling within a growing season", {
  
  # define soil properties
  soil <- data.table(
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  # define crop rotation
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  # define start_date
  parms <- list(
    start_date = "2022-07-01", # within the growing season
    unit = "A_SOM_LOI",
    poutput = "month"
  )
  
  result <- rc_sim(soil_properties = soil,
                   rothc_rotation = rothc_rotation,
                   rothc_parms = parms)
  
  expect_s3_class(result, "data.table")
  expect_gt(result[month == 11 & year == 2022, A_SOM_LOI], result[month == 10 & year == 2022, A_SOM_LOI])
})