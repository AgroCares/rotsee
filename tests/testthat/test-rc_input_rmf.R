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
  
  # Set time table 
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2030-01-01")
  
  # Update weather table
  weather <- rc_update_weather(dt = weather, dt.time = dt.time)
  
  # Check there are no errors with all valid input
  expect_no_error(rc_input_rmf(dt = rothc_rotation, B_DEPTH = B_DEPTH, A_CLAY_MI = A_CLAY_MI, dt.weather = weather, dt.time = dt.time))
  
 # Check that model runs when there is a accumulated soil moisture deficit in the starting month
  dt.time1 <- rc_time_period(start_date = "2022-05-01", end_date = "2030-01-01")
  
  expect_no_error(rc_input_rmf(dt = rothc_rotation, B_DEPTH = B_DEPTH, A_CLAY_MI = A_CLAY_MI, dt.weather = weather, dt.time = dt.time1))
})
