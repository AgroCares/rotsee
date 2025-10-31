# Test file for rc_input_event_amendment function
# Testing framework: testthat

library(testthat)
library(data.table)
testthat::source_file("helper-testdata.R")


# Test basic functionality with valid inputs
test_that("rc_input_event_amendment returns correct structure with valid inputs", {
  
  amendment <- create_event_amendment()
  
  dt.time <- rc_time_period(start_date = "2020-04-01", end_date = "2022-07-01")
  
  result <- rc_input_event_amendment(amendment = amendment, dt.time = dt.time)
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("time", "var", "value", "method") %in% colnames(result)))
  expect_true(all(result$method == "add"))
  expect_true(all(result$var %in% c("CDPM", "CRPM", "CHUM")))
  expect_type(result$time, "double")
  expect_type(result$value, "double")
})

# Test with NULL amendment (default case)
test_that("rc_input_event_amendment handles NULL amendment correctly", {
  dt.time <- rc_time_period(start_date = "2020-04-01", end_date = "2022-07-01")
  result <- rc_input_event_amendment(amendment = NULL, dt.time = dt.time)
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("time", "var", "value", "method") %in% colnames(result)))
  expect_equal(nrow(result), 0)
})

# Test input validation - amendment parameter
test_that("rc_input_event_amendment validates amendment parameter correctly", {
  valid_amendment <- create_event_amendment()
  
  dt.time <- rc_time_period(start_date = "2020-04-01", end_date = "2022-07-01")
  
  # Test with invalid column names
  invalid_amendment <- copy(valid_amendment)
  colnames(invalid_amendment)[colnames(invalid_amendment) == "year"] <- "invalid_name"

  expect_error(
    rc_input_event_amendment(invalid_amendment, dt.time),
    "Assertion"
  )
  
  # Test with negative cin_hum values
  negative_amendment <- copy(valid_amendment)[, cin_hum := c(-10, 150, 200)]
  
  expect_error(
    rc_input_event_amendment(negative_amendment, dt.time),
    "not >= 0"
  )
  
  # Test with excessive cin_tot values
  excessive_amendment <- copy(valid_amendment)[, cin_tot := c(150000, 1500, 2000)]

  expect_error(
    rc_input_event_amendment(excessive_amendment, dt.time),
    "not <= 100000"
  )
  
  
})



# Test with zero carbon inputs
test_that("rc_input_event_amendment handles zero carbon inputs correctly", {
  zero_amendment <- data.table(
    year = 2020,
    month = 5,
    cin_tot = 0,
    cin_hum = 0,
    cin_dpm = 0,
    cin_rpm = 0,
    fr_eoc_p = 15
  )
  
  dt.time <- rc_time_period(start_date = "2020-04-01", end_date = "2022-07-01")
  
  result <- rc_input_event_amendment(amendment = zero_amendment, dt.time = dt.time)

  expect_s3_class(result, "data.table")
})


# Test multiple years with different amendments
test_that("rc_input_event_amendment handles multiple years correctly", {
  
  multi_year_amendment <- data.table(
    year = 2020:2022,
    month = rep(5, each = 3),
    cin_tot = c(1000, 1500, 2000),
    cin_hum = c(100, 150, 200),
    cin_dpm = c(300, 450, 600),
    cin_rpm = c(600, 900, 1200),
    fr_eoc_p = c(15, 15, 15)
  )
  
  dt.time <- rc_time_period(start_date = "2020-04-01", end_date = "2022-07-01")
  
  result <- rc_input_event_amendment(multi_year_amendment, dt.time)
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
 
  # Should have entries for multiple years
  years_joined <- unique(merge(result[, .(time)], dt.time, by = "time")[, year])
  expect_true(all(years_joined %in% multi_year_amendment$year))
})

# Test melt operation and output structure
test_that("rc_input_event_amendment output has correct melted structure", {
  amendment <- create_event_amendment()
  
  dt.time <- rc_time_period(start_date = "2020-04-01", end_date = "2022-07-01")
  
  result <- rc_input_event_amendment(amendment, dt.time = dt.time)
  
  # Should have exactly these variable names after melting
  expected_vars <- c("CDPM", "CRPM", "CHUM")
  actual_vars <- unique(result$var)
  expect_true(all(expected_vars %in% actual_vars))
  
  # Each time point should have entries for all three variables
  time_var_counts <- result[, .N, by = .(time, var)]
  expect_true(all(time_var_counts$N > 0))
})

# Test boundary values for numeric inputs
test_that("rc_input_event_amendment handles boundary values correctly", {
  
  # Test minimum values
  min_amendment <- data.table(
    year = 2020,
    month = 5,
    cin_tot = 0,
    cin_hum = 0,
    cin_dpm = 0,
    cin_rpm = 0,
    fr_eoc_p = 0
  )
  dt.time <- rc_time_period(start_date = "2020-04-01", end_date = "2022-07-01")
  
  result_min <- rc_input_event_amendment(min_amendment, dt.time)
  
  expect_s3_class(result_min, "data.table")
  
  # Test maximum allowed values
  max_amendment <- data.table(
    year = 2020,
    month = 5,
    cin_tot = 100000,
    cin_hum = 100000,
    cin_dpm = 100000,
    cin_rpm = 100000,
    fr_eoc_p = 250
  )
  dt.time <- rc_time_period(start_date = "2020-04-01", end_date = "2022-07-01")
  
  result_max <- rc_input_event_amendment(max_amendment, dt.time)
  
  expect_s3_class(result_max, "data.table")
  expect_true(all(result_max$value >= 0))
})

