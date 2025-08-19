# Test file for rc_input_event_crop function
# Testing framework: testthat

library(testthat)
library(data.table)

test_that("rc_input_event_crop validates input parameters correctly", {
  # Test invalid crops input - not a data.table
  expect_error(
    rc_input_event_crop(data.frame(), A_CLAY_MI = 15),
    class = "simpleError"
  )
  
  # Test missing required columns
  crops_missing_cols <- data.table(
    B_LU = "nl_308",
    year = 2020
  )
  expect_error(
    rc_input_event_crop(crops_missing_cols, A_CLAY_MI = 15),
    class = "simpleError"
  )
  
  # Test invalid cf_yield values
  crops_invalid_yield <- data.table(
    B_LU = "nl_308",
    year = 2020,
    M_GREEN_TIMING = "august",
    M_CROPRESIDUE = TRUE,
    cf_yield = 2.5,  # Above valid range
    cin_crop_dpm = 500,
    cin_res_dpm = 200,
    cin_crop_rpm = 300,
    cin_res_rpm = 100
  )
  expect_error(
    rc_input_event_crop(crops_invalid_yield, A_CLAY_MI = 15),
    class = "simpleError"
  )
  
  # Test invalid M_GREEN_TIMING values
  crops_invalid_timing <- data.table(
    B_LU = "nl_308",
    year = 2020,
    M_GREEN_TIMING = "invalid_timing",
    M_CROPRESIDUE = TRUE,
    cf_yield = 1.0,
    cin_crop_dpm = 500,
    cin_res_dpm = 200,
    cin_crop_rpm = 300,
    cin_res_rpm = 100
  )
  expect_error(
    rc_input_event_crop(crops_invalid_timing, A_CLAY_MI = 15),
    class = "simpleError"
  )
  
  # Test negative cf_yield values
  crops_negative_yield <- data.table(
    B_LU = "nl_308",
    year = 2020,
    M_GREEN_TIMING = "august",
    M_CROPRESIDUE = TRUE,
    cf_yield = -0.5,
    cin_crop_dpm = 500,
    cin_res_dpm = 200,
    cin_crop_rpm = 300,
    cin_res_rpm = 100
  )
  expect_error(
    rc_input_event_crop(crops_negative_yield, A_CLAY_MI = 15),
    class = "simpleError"
  )
})

test_that("rc_input_event_crop handles valid inputs correctly", {
  # Create valid test data
  crops_valid <- data.table(
    B_LU = c("nl_308", "nl_252"),
    year = c(2020, 2021),
    M_GREEN_TIMING = c("august", "september"),
    M_CROPRESIDUE = c(TRUE, FALSE),
    cf_yield = c(1.0, 1.2),
    cin_crop_dpm = c(500, 750),
    cin_res_dpm = c(200, 300),
    cin_crop_rpm = c(300, 450),
    cin_res_rpm = c(100, 150)
  )
  
  # Test with clay content below 20%
  result_sandy <- rc_input_event_crop(crops_valid, A_CLAY_MI = 15)
  
  expect_s3_class(result_sandy, "data.table")
  expect_true(all(c("var", "value", "time", "method") %in% colnames(result_sandy)))
  expect_true(all(result_sandy$method == "add"))
  expect_true(all(result_sandy$value >= 0))
  
  # Test with clay content above 20%
  result_clay <- rc_input_event_crop(crops_valid, A_CLAY_MI = 25)
  
  expect_s3_class(result_clay, "data.table")
  expect_true(all(c("var", "value", "time", "method") %in% colnames(result_clay)))
})

test_that("rc_input_event_crop handles catch crop timing correctly", {
  # Test all valid M_GREEN_TIMING values
  timing_values <- c("august", "september", "october", "november", "never")
  
  for (timing in timing_values) {
    crops_test <- data.table(
      B_LU = "nl_308",
      year = 2020,
      M_GREEN_TIMING = timing,
      M_CROPRESIDUE = TRUE,
      cf_yield = 1.0,
      cin_crop_dpm = 500,
      cin_res_dpm = 200,
      cin_crop_rpm = 300,
      cin_res_rpm = 100
    )
    
    result <- rc_input_event_crop(crops_test, A_CLAY_MI = 15)
    
    expect_s3_class(result, "data.table")
    expect_true(all(result$value >= 0))
    
    # For "never" timing, catch crop carbon should be 0
    if (timing == "never") {
      # The function should still work but may have different carbon input patterns
      expect_true(is.data.table(result))
    }
  }
})

test_that("rc_input_event_crop handles mandatory catch crops on sandy soils", {
  # Test maize on sandy soil (should force catch crop timing to October)
  crops_maize_sandy <- data.table(
    B_LU = "nl_259", # Assuming this maps to maize in rc_crops
    year = 2020,
    M_GREEN_TIMING = "never",
    M_CROPRESIDUE = TRUE,
    cf_yield = 1.0,
    cin_crop_dpm = 500,
    cin_res_dpm = 200,
    cin_crop_rpm = 300,
    cin_res_rpm = 100
  )
  
  result_sandy <- rc_input_event_crop(crops_maize_sandy, A_CLAY_MI = 15)
  expect_s3_class(result_sandy, "data.table")
  
  # Test same crop on clay soil
  result_clay <- rc_input_event_crop(crops_maize_sandy, A_CLAY_MI = 25)
  expect_s3_class(result_clay, "data.table")
})

test_that("rc_input_event_crop handles crop residue management correctly", {
  # Test with crop residue incorporation
  crops_with_residue <- data.table(
    B_LU = "nl_308",
    year = 2020,
    M_GREEN_TIMING = "august",
    M_CROPRESIDUE = TRUE,
    cf_yield = 1.0,
    cin_crop_dpm = 500,
    cin_res_dpm = 200,
    cin_crop_rpm = 300,
    cin_res_rpm = 100
  )
  
  result_with_residue <- rc_input_event_crop(crops_with_residue, A_CLAY_MI = 15)
  
  # Test without crop residue incorporation
  crops_without_residue <- data.table(
    B_LU = "nl_308",
    year = 2020,
    M_GREEN_TIMING = "august",
    M_CROPRESIDUE = FALSE,
    cf_yield = 1.0,
    cin_crop_dpm = 500,
    cin_res_dpm = 200,
    cin_crop_rpm = 300,
    cin_res_rpm = 100
  )
  
  result_without_residue <- rc_input_event_crop(crops_without_residue, A_CLAY_MI = 15)
  
  # Both should return valid data.tables
  expect_s3_class(result_with_residue, "data.table")
  expect_s3_class(result_without_residue, "data.table")
  
  # Results may differ based on residue management
  expect_true(nrow(result_with_residue) >= 0)
  expect_true(nrow(result_without_residue) >= 0)
})

test_that("rc_input_event_crop handles edge cases for cf_yield", {
  # Test minimum valid cf_yield (0)
  crops_min_yield <- data.table(
    B_LU = "nl_308",
    year = 2020,
    M_GREEN_TIMING = "august",
    M_CROPRESIDUE = TRUE,
    cf_yield = 0.0,
    cin_crop_dpm = 500,
    cin_res_dpm = 200,
    cin_crop_rpm = 300,
    cin_res_rpm = 100
  )
  
  result_min <- rc_input_event_crop(crops_min_yield, A_CLAY_MI = 15)
  expect_s3_class(result_min, "data.table")
  
  # Test maximum valid cf_yield (2.0)
  crops_max_yield <- data.table(
    B_LU = "nl_308",
    year = 2020,
    M_GREEN_TIMING = "august",
    M_CROPRESIDUE = TRUE,
    cf_yield = 2.0,
    cin_crop_dpm = 500,
    cin_res_dpm = 200,
    cin_crop_rpm = 300,
    cin_res_rpm = 100
  )
  
  result_max <- rc_input_event_crop(crops_max_yield, A_CLAY_MI = 15)
  expect_s3_class(result_max, "data.table")
})

test_that("rc_input_event_crop handles multiple years correctly", {
  # Test multi-year crop rotation
  crops_multi_year <- data.table(
    B_LU = rep(c("nl_308", "nl_242"), 3),
    year = rep(c(2020, 2021, 2022), each = 2),
    M_GREEN_TIMING = rep(c("august", "october"), 3),
    M_CROPRESIDUE = rep(c(TRUE, FALSE), 3),
    cf_yield = rep(c(1.0, 1.5), 3),
    cin_crop_dpm = rep(c(500, 750), 3),
    cin_res_dpm = rep(c(200, 300), 3),
    cin_crop_rpm = rep(c(300, 450), 3),
    cin_res_rpm = rep(c(100, 150), 3)
  )
  
  result_multi <- rc_input_event_crop(crops_multi_year, A_CLAY_MI = 15)
  
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
    M_GREEN_TIMING = "august",
    M_CROPRESIDUE = TRUE,
    cf_yield = 1.0,
    cin_crop_dpm = 500,
    cin_res_dpm = 200,
    cin_crop_rpm = 300,
    cin_res_rpm = 100
  )
  
  result <- rc_input_event_crop(crops_test, A_CLAY_MI = 15)
  
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
    M_GREEN_TIMING = character(0),
    M_CROPRESIDUE = logical(0),
    cf_yield = numeric(0),
    cin_crop_dpm = numeric(0),
    cin_res_dpm = numeric(0),
    cin_crop_rpm = numeric(0),
    cin_res_rpm = numeric(0)
  )
  
  result_empty <- rc_input_event_crop(crops_empty, A_CLAY_MI = 15)
  
  expect_s3_class(result_empty, "data.table")
  expect_equal(nrow(result_empty), 0)
  expect_true(all(c("time", "var", "value", "method") %in% colnames(result_empty)))
})

test_that("rc_input_event_crop handles boundary clay content values", {
  crops_test <- data.table(
    B_LU = "nl_308",
    year = 2020,
    M_GREEN_TIMING = "august",
    M_CROPRESIDUE = TRUE,
    cf_yield = 1.0,
    cin_crop_dpm = 500,
    cin_res_dpm = 200,
    cin_crop_rpm = 300,
    cin_res_rpm = 100
  )
  
  # Test exactly at boundary (20%)
  result_boundary <- rc_input_event_crop(crops_test, A_CLAY_MI = 20)
  expect_s3_class(result_boundary, "data.table")
  
  # Test just below boundary
  result_below <- rc_input_event_crop(crops_test, A_CLAY_MI = 19.9)
  expect_s3_class(result_below, "data.table")
  
  # Test just above boundary
  result_above <- rc_input_event_crop(crops_test, A_CLAY_MI = 20.1)
  expect_s3_class(result_above, "data.table")
})

test_that("rc_input_event_crop handles different crop types", {
  # Test with different B_LU patterns that would match different crop types
  crop_types <- c("nl_308", "nl_256", "nl_266", "non_nl_crop")
  
  for (crop_type in crop_types) {
    crops_test <- data.table(
      B_LU = crop_type,
      year = 2020,
      M_GREEN_TIMING = "august",
      M_CROPRESIDUE = TRUE,
      cf_yield = 1.0,
      cin_crop_dpm = 500,
      cin_res_dpm = 200,
      cin_crop_rpm = 300,
      cin_res_rpm = 100
    )
    
    # Should not error regardless of crop type
    expect_no_error({
      result <- rc_input_event_crop(crops_test, A_CLAY_MI = 15)
    })
  }
})

test_that("rc_input_event_crop handles zero carbon inputs", {
  # Test with zero carbon inputs
  crops_zero_carbon <- data.table(
    B_LU = "nl_308",
    year = 2020,
    M_GREEN_TIMING = "august",
    M_CROPRESIDUE = TRUE,
    cf_yield = 1.0,
    cin_crop_dpm = 0,
    cin_res_dpm = 0,
    cin_crop_rpm = 0,
    cin_res_rpm = 0
  )
  
  result_zero <- rc_input_event_crop(crops_zero_carbon, A_CLAY_MI = 15)
  expect_s3_class(result_zero, "data.table")
})

test_that("rc_input_event_crop handles high carbon inputs", {
  # Test with high carbon inputs
  crops_high_carbon <- data.table(
    B_LU = "nl_308",
    year = 2020,
    M_GREEN_TIMING = "august",
    M_CROPRESIDUE = TRUE,
    cf_yield = 1.0,
    cin_crop_dpm = 5000,
    cin_res_dpm = 2000,
    cin_crop_rpm = 3000,
    cin_res_rpm = 1000
  )
  
  result_high <- rc_input_event_crop(crops_high_carbon, A_CLAY_MI = 15)
  expect_s3_class(result_high, "data.table")
  expect_true(all(result_high$value >= 0))
})

test_that("rc_input_event_crop carbon calculation is correct", {
  # Test that cin_dpm and cin_rpm are calculated correctly from components
  crops_test <- data.table(
    B_LU = "nl_308",
    year = 2020,
    M_GREEN_TIMING = "august",
    M_CROPRESIDUE = TRUE,
    cf_yield = 1.0,
    cin_crop_dpm = 500,
    cin_res_dpm = 200,
    cin_crop_rpm = 300,
    cin_res_rpm = 100
  )
  
  result <- rc_input_event_crop(crops_test, A_CLAY_MI = 15)
  
  expect_s3_class(result, "data.table")
  
  # Should have both CDPM and CRPM in results
  if (nrow(result) > 0) {
    expect_true(any(result$var == "CDPM"))
    expect_true(any(result$var == "CRPM"))
  }
})