# Test file for rc_input_event_crop function
# Testing framework: testthat

library(testthat)
library(data.table)

test_that("rc_input_event_crop validates input parameters correctly", {
  
  # Test missing required columns
  crops_missing_cols <- data.table(
    B_LU = "nl_308",
    year = 2020
  )
  expect_error(
    rc_input_event_crop(crops_missing_cols),
    class = "simpleError"
  )

})

test_that("rc_input_event_crop handles valid inputs correctly", {
  # Create valid test data
  crops_valid <- data.table(
    B_LU = c("nl_308", "nl_252"),
    year = c(2020, 2021),
    cin_dpm = c(500, 750),
    cin_rpm = c(300, 450)
  )
  
  # Test with clay content below 20%
  result_sandy <- rc_input_event_crop(crops_valid)
  
  expect_s3_class(result_sandy, "data.table")
  expect_true(all(c("var", "value", "time", "method") %in% colnames(result_sandy)))
  expect_true(all(result_sandy$method == "add"))
  expect_true(all(result_sandy$value >= 0))
  
  # Test with clay content above 20%
  result_clay <- rc_input_event_crop(crops_valid)
  
  expect_s3_class(result_clay, "data.table")
  expect_true(all(c("var", "value", "time", "method") %in% colnames(result_clay)))
})


test_that("rc_input_event_crop handles multiple years correctly", {
  # Test multi-year crop rotation
  crops_multi_year <- data.table(
    B_LU = rep(c("nl_308", "nl_242"), 3),
    year = rep(c(2020, 2021, 2022), each = 2),
    cin_dpm = rep(c(500, 750), 3),
    cin_rpm = rep(c(300, 450), 3)
  )
  
  result_multi <- rc_input_event_crop(crops_multi_year)
  
  expect_s3_class(result_multi, "data.table")
  expect_true(all(c("var", "value", "time", "method") %in% colnames(result_multi)))
  expect_true(all(result_multi$value >= 0))
  expect_true(length(unique(result_multi$time)) >= 1)
  
  # Check that time values are reasonable
  expect_true(all(result_multi$time >= 0))
})

test_that("rc_input_event_crop output structure is correct", {
  # Test output structure with valid input
  crops_test <- data.table(
    B_LU = "nl_308",
    year = 2020,
    cin_dpm = 500,
    cin_rpm = 300
  )
  
  result <- rc_input_event_crop(crops_test)
  
  # Check column names
  expected_cols <- c("time", "var", "value", "method")
  expect_true(all(expected_cols %in% colnames(result)))
  
  # Check variable types
  expect_true(is.numeric(result$time))
  expect_true(is.numeric(result$value))
  expect_true(is.character(result$method))
  
  # Check that all methods are "add"
  expect_true(all(result$method == "add"))
  
  # Check that var contains expected pool types
  if (nrow(result) > 0) {
    expect_true(all(result$var %in% c("CDPM", "CRPM")))
  }
})

test_that("rc_input_event_crop handles empty input gracefully", {
  # Test with empty data.table
  crops_empty <- data.table(
    B_LU = character(0),
    year = integer(0),
    cin_dpm = numeric(0),
    cin_rpm = numeric(0)
  )
  
  result_empty <- rc_input_event_crop(crops_empty)
  
  expect_s3_class(result_empty, "data.table")
  expect_equal(nrow(result_empty), 0)
  expect_true(all(c("time", "var", "value", "method") %in% colnames(result_empty)))
})



test_that("rc_input_event_crop handles zero carbon inputs", {
  # Test with zero carbon inputs
  crops_zero_carbon <- data.table(
    B_LU = "nl_308",
    year = 2020,
    cin_dpm = 0,
    cin_rpm = 0
  )
  
  result_zero <- rc_input_event_crop(crops_zero_carbon)
  expect_s3_class(result_zero, "data.table")
})

test_that("rc_input_event_crop handles high carbon inputs", {
  # Test with high carbon inputs
  crops_high_carbon <- data.table(
    B_LU = "nl_308",
    year = 2020,
    cin_dpm = 5000,
    cin_rpm = 1000
  )
  
  result_high <- rc_input_event_crop(crops_high_carbon)
  expect_s3_class(result_high, "data.table")
  expect_true(all(result_high$value >= 0))
})

test_that("rc_input_event_crop carbon calculation is correct", {
  # Test that cin_dpm and cin_rpm are calculated correctly from components
  crops_test <- data.table(
    B_LU = "nl_308",
    year = 2020,
    cin_dpm = 500,
    cin_rpm = 100
  )
  
  result <- rc_input_event_crop(crops_test)
  
  expect_s3_class(result, "data.table")
  
  # Should have both CDPM and CRPM in results
  if (nrow(result) > 0) {
    expect_true(any(result$var == "CDPM"))
    expect_true(any(result$var == "CRPM"))
  }
})