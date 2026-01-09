test_that("rc_input_rmf runs correctly", {
  # Set crop table
  rothc_rotation <- create_rotation()
  
  # Set B_depth
  B_DEPTH <- 0.3
  
  # Set clay content
  A_CLAY_MI <- 18
  
  # Set weather table
  weather <- create_weather()
  
  # Set irrigation moments
  irrigation <- create_irrigation()
  
  # Set time table 
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2030-01-01")
  
  # Update weather table
  weather <- rc_update_weather(dt = weather, dt.time = dt.time)
  
  # Check there are no errors with all valid input
  rc_input_rmf(dt = rothc_rotation,
              B_DEPTH = B_DEPTH,
              A_CLAY_MI = A_CLAY_MI,
              dt.weather = weather,
              dt.time = dt.time,
              dt.irrigation = irrigation)
  
 # Check that model runs when there is a accumulated soil moisture deficit in the starting month
  dt.time1 <- rc_time_period(start_date = "2022-05-01", end_date = "2030-01-01")
  
  weather1<- rc_update_weather(dt = weather, dt.time = dt.time1)
  
  result <- rc_input_rmf(dt = rothc_rotation,
                               B_DEPTH = B_DEPTH,
                               A_CLAY_MI = A_CLAY_MI,
                               dt.weather = weather1,
                               dt.time = dt.time1,
               dt.irrigation = irrigation)
  
  expect_type(result, "list")
  expect_true(all(c("R1", "abcd", "time") %in% names(result)))
  
  # Check that model runs when no irrigation is supplied
  result_no_irri <- rc_input_rmf(dt = rothc_rotation,
                               B_DEPTH = B_DEPTH,
                               A_CLAY_MI = A_CLAY_MI,
                               dt.weather = weather,
                               dt.time = dt.time)
  
  expect_type(result_no_irri, "list")
  expect_true(all(c("R1", "abcd", "time") %in% names(result_no_irri)))
  

})


test_that("rc_input_rmf validates irrigation input correctly", {
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  weather <- rc_update_weather(dt = weather, dt.time = dt.time)
  
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
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  weather <- rc_update_weather(dt = weather, dt.time = dt.time)
  
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
  
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  weather <- rc_update_weather(dt = weather, dt.time = dt.time)
  
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
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  # Multiple irrigation events across different months and years
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-05-01", "2022-07-01", "2022-08-15", "2023-06-01", "2023-07-15"),
    B_IRR_AMOUNT = c(15, 25, 20, 30, 18)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2024-01-01")
  
  weather <- rc_update_weather(dt = weather, dt.time = dt.time)
  
  result <- rc_input_rmf(
    dt = rothc_rotation,
    B_DEPTH = 0.3,
    A_CLAY_MI = 18,
    dt.weather = weather,
    dt.time = dt.time,
    dt.irrigation = irrigation
  )
  
  expect_type(result, "list")
  expect_true(all(c("R1", "abcd", "time") %in% names(result)))
})

test_that("rc_input_rmf handles irrigation dates correctly", {
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  weather <- rc_update_weather(dt = weather, dt.time = dt.time)
  
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
    , "format"
  )
})

test_that("rc_input_rmf handles NULL crop rotation with irrigation", {
  weather <- create_weather()
  
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(25)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  weather <- rc_update_weather(dt = weather, dt.time = dt.time)
  
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

test_that("rc_input_rmf uses W_ET_REFACT in ET calculations with irrigation", {
  rothc_rotation <- create_rotation()
  
  # Weather with custom W_ET_REFACT
  weather_custom <- create_weather()[, W_ET_REFACT := rep(0.8, 12)]
    
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01"),
    B_IRR_AMOUNT = c(25)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  weather <- rc_update_weather(dt = weather_custom, dt.time = dt.time)
  
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
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  weather <- rc_update_weather(dt = weather, dt.time = dt.time)
  
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
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  # Multiple irrigation events in the same month
  irrigation_same_month <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01", "2022-07-15", "2022-07-25"),
    B_IRR_AMOUNT = c(15, 20, 10)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  weather <- rc_update_weather(dt = weather, dt.time = dt.time)
  
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
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
  # Irrigation during crop cover
  irrigation_during_crop <- data.table(
    B_DATE_IRRIGATION = c("2022-06-01", "2022-07-01"),
    B_IRR_AMOUNT = c(25, 30)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-01-01")
  
  weather <- rc_update_weather(dt = weather, dt.time = dt.time)
  
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

# Tests for rc_input_rmf validation of W_ET_REFACT

test_that("rc_input_rmf validates W_ET_REFACT when provided", {
  # Setup minimal valid inputs
  dt_crop <- data.table(
    B_LU_START = "2022-04-01",
    B_LU_END = "2022-09-01"
  )
  
  dt_time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  # Valid W_ET_REFACT
  valid_weather <- data.table(
    year = 2022,
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_REFACT = rep(0.75, 12)
  )
  
  expect_no_error(
    rc_input_rmf(dt = dt_crop, A_CLAY_MI = 20, dt.weather = valid_weather, 
                 dt.time = dt_time)
  )
  
  # Invalid W_ET_REFACT - too high
  invalid_high <- copy(valid_weather)
  invalid_high[, W_ET_REFACT := 3.0]
  
  expect_error(
    rc_input_rmf(dt = dt_crop, A_CLAY_MI = 20, dt.weather = invalid_high, 
                 dt.time = dt_time),
    "W_ET_REFACT"
  )
  
  # Invalid W_ET_REFACT - too low
  invalid_low <- copy(valid_weather)
  invalid_low[, W_ET_REFACT := 0.1]
  
  expect_error(
    rc_input_rmf(dt = dt_crop, A_CLAY_MI = 20, dt.weather = invalid_low, 
                 dt.time = dt_time),
    "W_ET_REFACT"
  )
})

test_that("rc_input_rmf accepts W_ET_REFACT with NA values", {
  dt_crop <- data.table(
    B_LU_START = "2022-04-01",
    B_LU_END = "2022-09-01"
  )
  
  dt_time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  # W_ET_REFACT with some NA values (should be allowed)
  weather_with_na <- data.table(
    year = 2022,
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_REFACT = c(0.75, NA, 0.8, NA, rep(0.75, 8))
  )
  
  expect_no_error(
    rc_input_rmf(dt = dt_crop, A_CLAY_MI = 20, dt.weather = weather_with_na, 
                 dt.time = dt_time)
  )
})

test_that("rc_input_rmf works without W_ET_REFACT when W_ET_ACT_MONTH provided", {
  dt_crop <- data.table(
    B_LU_START = "2022-04-01",
    B_LU_END = "2022-09-01"
  )
  
  dt_time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  # Weather with W_ET_ACT_MONTH but no W_ET_REFACT
  weather_actual <- data.table(
    year = 2022,
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_ACT_MONTH = rep(40, 12)
  )
  
  expect_no_error(
    rc_input_rmf(dt = dt_crop, A_CLAY_MI = 20, dt.weather = weather_actual, 
                 dt.time = dt_time)
  )
})

test_that("rc_input_rmf validates W_ET_REFACT at boundary values", {
  dt_crop <- data.table(
    B_LU_START = "2022-04-01",
    B_LU_END = "2022-09-01"
  )
  
  dt_time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  # Test at lower boundary (0.3)
  weather_lower <- data.table(
    year = 2022,
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_REFACT = rep(0.3, 12)
  )
  
  expect_no_error(
    rc_input_rmf(dt = dt_crop, A_CLAY_MI = 20, dt.weather = weather_lower, 
                 dt.time = dt_time)
  )
  
  # Test at upper boundary (2.0)
  weather_upper <- data.table(
    year = 2022,
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_REFACT = rep(2.0, 12)
  )
  
  expect_no_error(
    rc_input_rmf(dt = dt_crop, A_CLAY_MI = 20, dt.weather = weather_upper, 
                 dt.time = dt_time)
  )
})

test_that("rc_input_rmf correctly throws error when columns do not have act or ref ET", {
  # set crop table
  dt_crop <- data.table(
    B_LU_START = "2022-04-01",
    B_LU_END = "2022-09-01"
  )
  
  # set time table
  dt_time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  # read in weather table
  weather <- create_weather()
  weather <- rc_update_weather(dt = weather, dt.time = dt_time)
  
  
  # Run with no ET_REF_MONTH column and no usable ET_ACT
  weather_noref <- copy(weather)[, W_ET_REF_MONTH := NULL]
  if ("W_ET_ACT_MONTH" %in% names(weather_noref)) weather_noref[, W_ET_ACT_MONTH := NA_real_]
  expect_error(rc_input_rmf(dt = dt_crop, A_CLAY_MI = 20, dt.weather = weather_noref, dt.time = dt_time),
               "W_ET_REF_MONTH is not available")
  

})
