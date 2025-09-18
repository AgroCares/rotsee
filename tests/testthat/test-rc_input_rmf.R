test_that("rc_input_rmf runs correctly", {
  # Set crop table
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)", "erwten (droog te oogsten)" ),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  # Set B_depth
  B_DEPTH <- 0.3
  
  # Set clay content
  A_CLAY_MI <- 18
  
  # Set weather table
  weather <- data.table(month = 1:12,
                        W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                        W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                        W_ET_POT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                        W_ET_ACT_MONTH = NA_real_,
                        W_POT_TO_ACT = rep(0.75, 12))
  
  # Set irrigation moments
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01", "2024-07-01", "2028-06-01"),
    B_IRR_AMOUNT = c(12, 20, 10)
  )
  
  
  # Set time table 
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2030-01-01")
  
  # Check there are no errors with all valid input
 rc_input_rmf(dt = rothc_rotation,
              B_DEPTH = B_DEPTH,
              A_CLAY_MI = A_CLAY_MI,
              dt.weather = weather,
              dt.time = dt.time,
              dt.irrigation = irrigation)
  
 # Check that model runs when there is a accumulated soil moisture deficit in the starting month
  dt.time1 <- rc_time_period(start_date = "2022-05-01", end_date = "2030-01-01")
  
  expect_no_error(rc_input_rmf(dt = rothc_rotation,
                               B_DEPTH = B_DEPTH,
                               A_CLAY_MI = A_CLAY_MI,
                               dt.weather = weather,
                               dt.time = dt.time1,
               dt.irrigation = irrigation))
  
  # Check that model runs when no irrigation is supplied
  expect_no_error(rc_input_rmf(dt = rothc_rotation,
                               B_DEPTH = B_DEPTH,
                               A_CLAY_MI = A_CLAY_MI,
                               dt.weather = weather,
                               dt.time = dt.time))
  
  
})

