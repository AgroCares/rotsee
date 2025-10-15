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


# ====================================================================
# NEW TESTS FOR IRRIGATION FUNCTIONALITY (AddIrrigation branch)
# ====================================================================

test_that("rc_input_rmf handles irrigation data correctly", {
  # Setup test data
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)", "erwten (droog te oogsten)"),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2),
    W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
    W_ET_POT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),
    W_ET_ACT_MONTH = NA_real_,
    W_POT_TO_ACT = rep(0.75, 12)
  )
  
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01", "2023-07-01"),
    B_IRR_AMOUNT = c(25, 30)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2024-01-01")
  
  result <- rc_input_rmf(
    dt = rothc_rotation,
    B_DEPTH = 0.3,
    A_CLAY_MI = 18,
    dt.weather = weather,
    dt.time = dt.time,
    dt.irrigation = irrigation
  )
  
  # Check result structure
  expect_type(result, "list")
  expect_true(all(c("R1", "abc", "time") %in% names(result)))
})

test_that("rc_input_rmf validates irrigation input correctly", {
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_POT_MONTH = rep(50, 12),
    W_POT_TO_ACT = rep(0.75, 12)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  # Missing B_DATE_IRRIGATION column
  invalid_irrigation1 <- data.table(
    B_IRR_AMOUNT = c(25, 30)
  )
  
  expect_error(
    rc_input_rmf(
      dt = rothc_rotation,
      B_DEPTH = 0.3,
      A_CLAY_MI = 18,
      dt.weather = weather,
      dt.time = dt.time,
      dt.irrigation = invalid_irrigation1
    ),
    "B_DATE_IRRIGATION"
  )
  
  # Missing B_IRR_AMOUNT column
  invalid_irrigation2 <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01", "2023-07-01")
  )
  
  expect_error(
    rc_input_rmf(
      dt = rothc_rotation,
      B_DEPTH = 0.3,
      A_CLAY_MI = 18,
      dt.weather = weather,
      dt.time = dt.time,
      dt.irrigation = invalid_irrigation2
    ),
    "B_IRR_AMOUNT"
  )
})

test_that("rc_input_rmf validates irrigation amount ranges", {
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_POT_MONTH = rep(50, 12),
    W_POT_TO_ACT = rep(0.75, 12)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  # Negative irrigation amount
  invalid_irrigation_negative <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(-10)
  )
  
  expect_error(
    rc_input_rmf(
      dt = rothc_rotation,
      B_DEPTH = 0.3,
      A_CLAY_MI = 18,
      dt.weather = weather,
      dt.time = dt.time,
      dt.irrigation = invalid_irrigation_negative
    ),
    "B_IRR_AMOUNT"
  )
  
  # Excessive irrigation amount (over 1000 mm)
  invalid_irrigation_high <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(1500)
  )
  
  expect_error(
    rc_input_rmf(
      dt = rothc_rotation,
      B_DEPTH = 0.3,
      A_CLAY_MI = 18,
      dt.weather = weather,
      dt.time = dt.time,
      dt.irrigation = invalid_irrigation_high
    ),
    "B_IRR_AMOUNT"
  )
})

test_that("rc_input_rmf handles boundary irrigation amounts", {
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_POT_MONTH = rep(50, 12),
    W_POT_TO_ACT = rep(0.75, 12)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  # Zero irrigation amount
  irrigation_zero <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(0)
  )
  
  expect_no_error(
    rc_input_rmf(
      dt = rothc_rotation,
      B_DEPTH = 0.3,
      A_CLAY_MI = 18,
      dt.weather = weather,
      dt.time = dt.time,
      dt.irrigation = irrigation_zero
    )
  )
  
  # Maximum irrigation amount (1000 mm)
  irrigation_max <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(1000)
  )
  
  expect_no_error(
    rc_input_rmf(
      dt = rothc_rotation,
      B_DEPTH = 0.3,
      A_CLAY_MI = 18,
      dt.weather = weather,
      dt.time = dt.time,
      dt.irrigation = irrigation_max
    )
  )
})

test_that("rc_input_rmf handles multiple irrigation events", {
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_POT_MONTH = rep(50, 12),
    W_POT_TO_ACT = rep(0.75, 12)
  )
  
  # Multiple irrigation events across different months and years
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-05-01", "2022-07-01", "2022-08-15", "2023-06-01", "2023-07-15"),
    B_IRR_AMOUNT = c(15, 25, 20, 30, 18)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2024-01-01")
  
  result <- rc_input_rmf(
    dt = rothc_rotation,
    B_DEPTH = 0.3,
    A_CLAY_MI = 18,
    dt.weather = weather,
    dt.time = dt.time,
    dt.irrigation = irrigation
  )
  
  expect_type(result, "list")
  expect_true(all(c("R1", "abc", "time") %in% names(result)))
})

test_that("rc_input_rmf handles irrigation dates correctly", {
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_POT_MONTH = rep(50, 12),
    W_POT_TO_ACT = rep(0.75, 12)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  # Invalid date format
  invalid_date <- data.table(
    B_DATE_IRRIGATION = c("not-a-date"),
    B_IRR_AMOUNT = c(25)
  )
  
  expect_error(
    rc_input_rmf(
      dt = rothc_rotation,
      B_DEPTH = 0.3,
      A_CLAY_MI = 18,
      dt.weather = weather,
      dt.time = dt.time,
      dt.irrigation = invalid_date
    )
  )
})

test_that("rc_input_rmf handles NULL crop rotation with irrigation", {
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_POT_MONTH = rep(50, 12),
    W_POT_TO_ACT = rep(0.75, 12)
  )
  
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(25)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  # Should work with NULL crop rotation
  expect_no_error(
    rc_input_rmf(
      dt = NULL,
      B_DEPTH = 0.3,
      A_CLAY_MI = 18,
      dt.weather = weather,
      dt.time = dt.time,
      dt.irrigation = irrigation
    )
  )
})

test_that("rc_input_rmf uses W_POT_TO_ACT in ET calculations with irrigation", {
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  # Weather with custom W_POT_TO_ACT
  weather_custom <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_POT_MONTH = rep(100, 12),
    W_ET_ACT_MONTH = NA_real_,
    W_POT_TO_ACT = rep(0.8, 12)  # Custom factor
  )
  
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(25)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  result <- rc_input_rmf(
    dt = rothc_rotation,
    B_DEPTH = 0.3,
    A_CLAY_MI = 18,
    dt.weather = weather_custom,
    dt.time = dt.time,
    dt.irrigation = irrigation
  )
  
  # Should complete without errors
  expect_type(result, "list")
})

test_that("rc_input_rmf handles irrigation at different times of year", {
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_POT_MONTH = rep(50, 12),
    W_POT_TO_ACT = rep(0.75, 12)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  # Irrigation in winter month
  irrigation_winter <- data.table(
    B_DATE_IRRIGATION = c("2022-01-15"),
    B_IRR_AMOUNT = c(10)
  )
  
  expect_no_error(
    rc_input_rmf(
      dt = rothc_rotation,
      B_DEPTH = 0.3,
      A_CLAY_MI = 18,
      dt.weather = weather,
      dt.time = dt.time,
      dt.irrigation = irrigation_winter
    )
  )
  
  # Irrigation in summer month
  irrigation_summer <- data.table(
    B_DATE_IRRIGATION = c("2022-07-15"),
    B_IRR_AMOUNT = c(50)
  )
  
  expect_no_error(
    rc_input_rmf(
      dt = rothc_rotation,
      B_DEPTH = 0.3,
      A_CLAY_MI = 18,
      dt.weather = weather,
      dt.time = dt.time,
      dt.irrigation = irrigation_summer
    )
  )
})

test_that("rc_input_rmf handles same month multiple irrigation events", {
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_POT_MONTH = rep(50, 12),
    W_POT_TO_ACT = rep(0.75, 12)
  )
  
  # Multiple irrigation events in the same month
  irrigation_same_month <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01", "2022-07-15", "2022-07-25"),
    B_IRR_AMOUNT = c(15, 20, 10)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  # Should handle and aggregate correctly
  expect_no_error(
    rc_input_rmf(
      dt = rothc_rotation,
      B_DEPTH = 0.3,
      A_CLAY_MI = 18,
      dt.weather = weather,
      dt.time = dt.time,
      dt.irrigation = irrigation_same_month
    )
  )
})

test_that("rc_input_rmf irrigation integration with crop cover", {
  # Test irrigation during and outside crop cover period
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-09-30"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  weather <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_POT_MONTH = rep(50, 12),
    W_POT_TO_ACT = rep(0.75, 12)
  )
  
  # Irrigation during crop cover
  irrigation_during_crop <- data.table(
    B_DATE_IRRIGATION = c("2022-06-01", "2022-07-01"),
    B_IRR_AMOUNT = c(25, 30)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  result_during <- rc_input_rmf(
    dt = rothc_rotation,
    B_DEPTH = 0.3,
    A_CLAY_MI = 18,
    dt.weather = weather,
    dt.time = dt.time,
    dt.irrigation = irrigation_during_crop
  )
  
  expect_type(result_during, "list")
  
  # Irrigation outside crop cover
  irrigation_outside_crop <- data.table(
    B_DATE_IRRIGATION = c("2022-01-15", "2022-11-15"),
    B_IRR_AMOUNT = c(15, 20)
  )
  
  result_outside <- rc_input_rmf(
    dt = rothc_rotation,
    B_DEPTH = 0.3,
    A_CLAY_MI = 18,
    dt.weather = weather,
    dt.time = dt.time,
    dt.irrigation = irrigation_outside_crop
  )
  
  expect_type(result_outside, "list")
})