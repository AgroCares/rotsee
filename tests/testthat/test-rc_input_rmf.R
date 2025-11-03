testthat::source_file("helper-testdata.R")

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
  expect_true(all(c("R1", "abc", "time") %in% names(result)))
  
  # Check that model runs when no irrigation is supplied
  result_no_irri <- rc_input_rmf(dt = rothc_rotation,
                               B_DEPTH = B_DEPTH,
                               A_CLAY_MI = A_CLAY_MI,
                               dt.weather = weather,
                               dt.time = dt.time)
  
  expect_type(result_no_irri, "list")
  expect_true(all(c("R1", "abc", "time") %in% names(result_no_irri)))
  
  
  
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
  expect_true(all(c("R1", "abc", "time") %in% names(result)))
})

test_that("rc_input_rmf handles irrigation dates correctly", {
  rothc_rotation <- create_rotation()
  
  weather <- create_weather()
  
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