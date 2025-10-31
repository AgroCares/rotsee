# Unit tests to check if parallel rothc

test_that("rc_sim_multi runs with normal inputs", {
  soil_properties <- data.table(
    ID = c('high', 'low', 'mid'),
    A_C_OF = rep(50,3),
    B_C_ST03 = rep(210,3),
    A_CLAY_MI = rep(18,3),
    A_DENSITY_SA = rep(1.4,3)
  )
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3
  
  rothc_rotation <- data.table(
    ID = rep(c('high', 'low', 'mid'),2),
    B_LU_START = rep(c("2022-04-01", "2023-04-01"),each=3),
    B_LU_END = rep(c("2022-10-01", "2023-10-01"),each=3),
    B_LU = rep(c("nl_308", "nl_308"),each = 3),
    B_LU_NAME = rep(c("erwten (droog te oogsten)", "erwten (droog te oogsten)" ),each =3),
    B_LU_HC = rep(0.32, 6),
    B_C_OF_INPUT = rep(1500, 6)
  )
  
  rothc_amendment <- data.table(
    ID = rep(c('high', 'low', 'mid'),2),
    P_ID = rep(1, 6),
    P_NAME = rep('cattle_slurry', 6),
    P_DOSE = rep(c(100000, 25000, 70000), 2),
    P_HC = rep(0.7,6),
    P_C_OF = rep(35, 6),
    P_DATE_FERTILIZATION = rep(c("2022-05-01", "2023-05-01"),each=3)
  )
  
  
  weather <- data.table(month = 1:12,
                        W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                        W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                        W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                        W_ET_ACT_MONTH = NA_real_)
  
  parms <- list(dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
                c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
                initialize = TRUE,
                unit = "A_SOM_LOI",
                method = "adams",
                poutput = "year",
                start_date = "2022-04-01",
                end_date = "2040-10-01")
  
  
  # Run with all correct values
  result <- rc_sim_multi(soil_properties = soil_properties,
                         A_DEPTH = 0.3,
                         B_DEPTH = 0.3,
                         parms = parms,
                         weather = weather,
                         rotation = rothc_rotation,
                         amendment = rothc_amendment,
                         final = FALSE,
                         strategy = 'multisession')

  expect_s3_class(result, "data.table")
  expect_true(all(c("ID", "A_SOM_LOI", "soc", "xs") %in% names(result)))
  expect_equal(nrow(result), 57)


  # Runs correctly with final set to true
  result_final <- rc_sim_multi(soil_properties = soil_properties,
                         A_DEPTH = 0.3,
                         B_DEPTH = 0.3,
                         parms = parms,
                         weather = weather,
                         rotation = rothc_rotation,
                         amendment = rothc_amendment,
                         final = TRUE,
                         strategy = 'multisession')

  
  expect_equal(nrow(result_final), 3)
})


test_that("rc_sim_multi throws error if soil_properties is not a data.table", {
  expect_error(rc_sim_multi(soil_properties = data.frame(ID = "test")))
})


test_that("rc_sim_multi handles progress reporting", {
  soil_properties <- data.table(
    ID = c('high', 'low', 'mid'),
    A_C_OF = rep(50,3),
    B_C_ST03 = rep(210,3),
    A_CLAY_MI = rep(18,3),
    A_DENSITY_SA = rep(1.4,3)
  )
  
  rothc_rotation <- data.table(
    ID = rep(c('high', 'low', 'mid'),2),
    B_LU_START = rep(c("2022-04-01", "2023-04-01"),each=3),
    B_LU_END = rep(c("2022-10-01", "2023-10-01"),each=3),
    B_LU = rep(c("nl_308", "nl_308"),each = 3),
    B_LU_NAME = rep(c("erwten (droog te oogsten)", "erwten (droog te oogsten)" ),each=3),
    B_LU_HC = rep(0.32, 6),
    B_C_OF_INPUT = rep(1500, 6)
  )
  
  rothc_amendment <- data.table(
    ID = rep(c('high', 'low', 'mid'),2),
    P_ID = rep(1, 6),
    P_NAME = rep('cattle_slurry', 6),
    P_DOSE = rep(c(100000, 25000, 70000), 2),
    P_HC = rep(0.7,6),
    P_C_OF = rep(35, 6),
    P_DATE_FERTILIZATION = rep(c("2022-05-01", "2023-05-01"),each=3)
  )
  
  weather <- data.table(month = 1:12,
                        W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                        W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                        W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                        W_ET_ACT_MONTH = NA_real_)
  
  parms <- list(dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
                c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
                initialize = TRUE,
                simyears = 50,
                unit = "A_SOM_LOI",
                method = "adams",
                poutput = "year",
                start_date = "2022-04-01",
                end_date = "2040-10-01")
  
  # Test with progress reporting
  result <- rc_sim_multi(soil_properties = soil_properties,
                         A_DEPTH = 0.3,
                         B_DEPTH = 0.3,
                         parms = parms,
                         weather = weather,
                         rotation = rothc_rotation,
                         amendment = rothc_amendment,
                         quiet = FALSE,
                         strategy = 'multisession')
  
  expect_s3_class(result, "data.table")
})


