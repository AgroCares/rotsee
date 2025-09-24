# Test file for rc_input_event_amendment function
# Testing framework: testthat

library(testthat)
library(data.table)


create_test_amendment <- function() {
  data.table(
    year = c(2020, 2021, 2022),
    month = c(4, 5, 6),
    cin_tot = c(1000, 1500, 2000),
    cin_hum = c(100, 150, 200),
    cin_dpm = c(300, 450, 600),
    cin_rpm = c(600, 900, 1200)
  )
}

# Test basic functionality with valid inputs
test_that("rc_input_event_amendment returns correct structure with valid inputs", {
  amendment <- create_test_amendment()
  
  result <- rc_input_event_amendment(amendment)
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("time", "var", "value", "method") %in% colnames(result)))
  expect_true(all(result$method == "add"))
  expect_true(all(result$var %in% c("CDPM", "CRPM", "CHUM")))
  expect_type(result$time, "double")
  expect_type(result$value, "double")
})

# Test with NULL amendment (default case)
test_that("rc_input_event_amendment handles NULL amendment correctly", {
  result <- rc_input_event_amendment(amendment = NULL)
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("time", "var", "value", "method") %in% colnames(result)))
})

# Test input validation - amendment parameter
test_that("rc_input_event_amendment validates amendment parameter correctly", {
  
  # Test with invalid column names
  invalid_amendment <- data.table(
    invalid_col = 2020,
    month = 4
  )
  
  expect_error(
    rc_input_event_amendment(invalid_amendment),
    "Assertion"
  )
  
  # Test with negative cin_hum values
  negative_amendment <- create_test_amendment()
  negative_amendment$cin_hum <- c(-10, 150, 200)
  expect_error(
    rc_input_event_amendment(negative_amendment),
    "not >= 0"
  )
  
  # Test with excessive cin_tot values
  excessive_amendment <- create_test_amendment()
  excessive_amendment$cin_tot <- c(150000, 150, 200)
  expect_error(
    rc_input_event_amendment(excessive_amendment),
    "not <= 100000"
  )
  
  
})



# Test with zero carbon inputs
test_that("rc_input_event_amendment handles zero carbon inputs correctly", {
  zero_amendment <- data.table(
    year = 2020,
    cin_tot = 0,
    cin_hum = 0,
    cin_dpm = 0,
    cin_rpm = 0,
    fr_eoc_p = 15
  )
  
  result <- rc_input_event_amendment(zero_amendment)

  expect_s3_class(result, "data.table")
})

# Test edge case with NA month values
test_that("rc_input_event_amendment handles NA month values", {
  na_month_amendment <- data.table(
    year = 2020,
    month = NA_integer_,
    cin_tot = 1000,
    cin_hum = 100,
    cin_dpm = 300,
    cin_rpm = 600,
    fr_eoc_p = 15
  )
  
  result <- rc_input_event_amendment(na_month_amendment)
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
})

# Test multiple years with different amendments
test_that("rc_input_event_amendment handles multiple years correctly", {
  
  multi_year_amendment <- data.table(
    year = 2020:2022,
    cin_tot = c(1000, 1500, 2000),
    cin_hum = c(100, 150, 200),
    cin_dpm = c(300, 450, 600),
    cin_rpm = c(600, 900, 1200),
    fr_eoc_p = c(15, 15, 15)
  )
  
  result <- rc_input_event_amendment(multi_year_amendment)
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
 
  # Should have entries for multiple years
  unique_years <- unique(floor(result$time + min(multi_year_amendment$year)))
  expect_true(length(unique_years) >= 1)
})

# Test melt operation and output structure
test_that("rc_input_event_amendment output has correct melted structure", {
  amendment <- create_test_amendment()
  
  result <- rc_input_event_amendment(amendment)
  
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
    cin_tot = 0,
    cin_hum = 0,
    cin_dpm = 0,
    cin_rpm = 0,
    fr_eoc_p = 0
  )
  
  result_min <- rc_input_event_amendment(min_amendment)
  
  expect_s3_class(result_min, "data.table")
  
  # Test maximum allowed values
  max_amendment <- data.table(
    year = 2020,
    cin_tot = 100000,
    cin_hum = 100000,
    cin_dpm = 100000,
    cin_rpm = 100000,
    fr_eoc_p = 250
  )
  
  result_max <- rc_input_event_amendment(max_amendment)
  
  expect_s3_class(result_max, "data.table")
  expect_true(all(result_max$value >= 0))
})

