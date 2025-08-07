# Test file for rc_input_event_amendment function
# Testing framework: testthat

library(testthat)
library(data.table)

# Mock rc_crops data for testing (based on expected structure)
mock_rc_crops <- data.table(
  crop_code = c("101", "102", "103", "104", "105", "106"),
  crop_name = c("winter tarwe", "zomer tarwe", "gras", "mais", "aardappel", "suikerbiet"),
  stringsAsFactors = FALSE
)

# Setup function to create test data
create_test_crops <- function() {
  data.table(
    B_LU = c("nl_101", "nl_102", "nl_103"),
    year = c(2020, 2021, 2022)
  )
}

create_test_amendment <- function() {
  data.table(
    year = c(2020, 2021, 2022),
    month = c(4, 5, 6),
    cin_tot = c(1000, 1500, 2000),
    cin_hum = c(100, 150, 200),
    cin_dpm = c(300, 450, 600),
    cin_rpm = c(600, 900, 1200),
    fr_eoc_p = c(15, 25, 35)
  )
}

# Mock the rotsee::rc_crops data
with_mock_rc_crops <- function(code) {
  # Create a temporary environment with mock data
  old_crops <- NULL
  if (exists("rc_crops", envir = asNamespace("rotsee"))) {
    old_crops <- get("rc_crops", envir = asNamespace("rotsee"))
  }
  
  # Set mock data
  assign("rc_crops", mock_rc_crops, envir = asNamespace("rotsee"))
  
  # Execute the code
  tryCatch({
    result <- code
    return(result)
  }, finally = {
    # Restore original data if it existed
    if (!is.null(old_crops)) {
      assign("rc_crops", old_crops, envir = asNamespace("rotsee"))
    }
  })
}

# Test basic functionality with valid inputs
test_that("rc_input_event_amendment returns correct structure with valid inputs", {
  crops <- create_test_crops()
  amendment <- create_test_amendment()
  
  result <- with_mock_rc_crops({
    rc_input_event_amendment(crops, amendment)
  })
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("time", "var", "value", "method") %in% colnames(result)))
  expect_true(all(result$method == "add"))
  expect_true(all(result$var %in% c("CDPM", "CRPM", "CHUM")))
  expect_is(result$time, "numeric")
  expect_is(result$value, "numeric")
})

# Test with NULL amendment (default case)
test_that("rc_input_event_amendment handles NULL amendment correctly", {
  crops <- create_test_crops()
  
  result <- with_mock_rc_crops({
    rc_input_event_amendment(crops, amendment = NULL)
  })
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) >= 1)  # Should have at least one row due to default fallback
  expect_true(all(c("time", "var", "value", "method") %in% colnames(result)))
})

# Test input validation - crops parameter
test_that("rc_input_event_amendment validates crops parameter correctly", {
  amendment <- create_test_amendment()
  
  # Test with non-data.table crops
  expect_error(
    with_mock_rc_crops({
      rc_input_event_amendment(data.frame(B_LU = "nl_101", year = 2020), amendment)
    }),
    "data.table"
  )
  
  # Test with missing B_LU column
  crops_no_blu <- data.table(year = 2020)
  expect_error(
    with_mock_rc_crops({
      rc_input_event_amendment(crops_no_blu, amendment)
    }),
    "B_LU"
  )
  
  # Test with missing year column
  crops_no_year <- data.table(B_LU = "nl_101")
  expect_error(
    with_mock_rc_crops({
      rc_input_event_amendment(crops_no_year, amendment)
    }),
    "year"
  )
})

# Test input validation - amendment parameter
test_that("rc_input_event_amendment validates amendment parameter correctly", {
  crops <- create_test_crops()
  
  # Test with invalid column names
  invalid_amendment <- data.table(
    invalid_col = 2020,
    month = 4
  )
  expect_error(
    with_mock_rc_crops({
      rc_input_event_amendment(crops, invalid_amendment)
    }),
    "subset"
  )
  
  # Test with negative cin_hum values
  negative_amendment <- create_test_amendment()
  negative_amendment$cin_hum <- c(-10, 150, 200)
  expect_error(
    with_mock_rc_crops({
      rc_input_event_amendment(crops, negative_amendment)
    }),
    "lower = 0"
  )
  
  # Test with excessive cin_tot values
  excessive_amendment <- create_test_amendment()
  excessive_amendment$cin_tot <- c(150000, 150, 200)
  expect_error(
    with_mock_rc_crops({
      rc_input_event_amendment(crops, excessive_amendment)
    }),
    "upper = 100000"
  )
  
  # Test with excessive fr_eoc_p values
  excessive_fr_amendment <- create_test_amendment()
  excessive_fr_amendment$fr_eoc_p <- c(15, 300, 35)
  expect_error(
    with_mock_rc_crops({
      rc_input_event_amendment(crops, excessive_fr_amendment)
    }),
    "upper = 250"
  )
})

# Test fr_eoc_p categorization
test_that("rc_input_event_amendment categorizes amendments correctly based on fr_eoc_p", {
  crops <- create_test_crops()
  
  # Test high fr_eoc_p (should be autumn)
  high_fr_amendment <- data.table(
    year = 2020,
    cin_tot = 1000,
    cin_hum = 100,
    cin_dpm = 300,
    cin_rpm = 600,
    fr_eoc_p = 25  # > 20, should be autumn
  )
  
  result_high <- with_mock_rc_crops({
    rc_input_event_amendment(crops[1], high_fr_amendment)
  })
  
  # For autumn amendments, should see October timing (month 10)
  expect_true(any(result_high$time %% 1 == 10/12 - 1/12))  # October timing
  
  # Test low fr_eoc_p (should be spring)
  low_fr_amendment <- data.table(
    year = 2020,
    cin_tot = 1000,
    cin_hum = 100,
    cin_dpm = 300,
    cin_rpm = 600,
    fr_eoc_p = 15  # <= 20, should be spring
  )
  
  result_low <- with_mock_rc_crops({
    rc_input_event_amendment(crops[1], low_fr_amendment)
  })
  
  # For spring amendments, should see April timing (month 4) for non-grass
  expect_true(any(result_low$time %% 1 == 4/12 - 1/12))  # April timing
})

# Test grassland-specific timing
test_that("rc_input_event_amendment handles grassland timing correctly", {
  # Create grassland crops
  grass_crops <- data.table(
    B_LU = "nl_103",  # Assuming this maps to grass
    year = 2020
  )
  
  spring_amendment <- data.table(
    year = 2020,
    cin_tot = 1000,
    cin_hum = 100,
    cin_dpm = 300,
    cin_rpm = 600,
    fr_eoc_p = 15  # Spring category
  )
  
  result <- with_mock_rc_crops({
    rc_input_event_amendment(grass_crops, spring_amendment)
  })
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  
  # Should have multiple months for grassland spring amendments
  unique_months <- unique(round((result$time %% 1) * 12) + 1)
  expect_true(length(unique_months) > 1)  # Multiple application months
})

# Test winter wheat specific timing
test_that("rc_input_event_amendment handles winter wheat timing correctly", {
  # Create winter wheat crops
  wheat_crops <- data.table(
    B_LU = "nl_101",  # Assuming this maps to winter tarwe
    year = 2020
  )
  
  autumn_amendment <- data.table(
    year = 2020,
    cin_tot = 1000,
    cin_hum = 100,
    cin_dpm = 300,
    cin_rpm = 600,
    fr_eoc_p = 25  # Autumn category
  )
  
  result <- with_mock_rc_crops({
    rc_input_event_amendment(wheat_crops, autumn_amendment)
  })
  
  # Winter wheat should have September timing (month 9)
  september_timing <- 9/12 - 1/12
  expect_true(any(abs(result$time %% 1 - september_timing) < 0.01))
})

# Test amendment without month column
test_that("rc_input_event_amendment works without month column in amendment", {
  crops <- create_test_crops()
  amendment_no_month <- data.table(
    year = 2020,
    cin_tot = 1000,
    cin_hum = 100,
    cin_dpm = 300,
    cin_rpm = 600,
    fr_eoc_p = 15
  )
  
  result <- with_mock_rc_crops({
    rc_input_event_amendment(crops[1], amendment_no_month)
  })
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  expect_true(all(c("time", "var", "value", "method") %in% colnames(result)))
})

# Test with zero carbon inputs
test_that("rc_input_event_amendment handles zero carbon inputs correctly", {
  crops <- create_test_crops()
  zero_amendment <- data.table(
    year = 2020,
    cin_tot = 0,
    cin_hum = 0,
    cin_dpm = 0,
    cin_rpm = 0,
    fr_eoc_p = 15
  )
  
  result <- with_mock_rc_crops({
    rc_input_event_amendment(crops[1], zero_amendment)
  })
  
  # Should still return at least one row due to fallback
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) >= 1)
})

# Test edge case with NA month values
test_that("rc_input_event_amendment handles NA month values", {
  crops <- create_test_crops()
  na_month_amendment <- data.table(
    year = 2020,
    month = NA_integer_,
    cin_tot = 1000,
    cin_hum = 100,
    cin_dpm = 300,
    cin_rpm = 600,
    fr_eoc_p = 15
  )
  
  result <- with_mock_rc_crops({
    rc_input_event_amendment(crops[1], na_month_amendment)
  })
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  # NA months should default to month 4 (April)
})

# Test multiple years with different amendments
test_that("rc_input_event_amendment handles multiple years correctly", {
  crops <- data.table(
    B_LU = rep("nl_101", 3),
    year = 2020:2022
  )
  
  multi_year_amendment <- data.table(
    year = 2020:2022,
    cin_tot = c(1000, 1500, 2000),
    cin_hum = c(100, 150, 200),
    cin_dpm = c(300, 450, 600),
    cin_rpm = c(600, 900, 1200),
    fr_eoc_p = c(15, 15, 15)
  )
  
  result <- with_mock_rc_crops({
    rc_input_event_amendment(crops, multi_year_amendment)
  })
  
  expect_s3_class(result, "data.table")
  expect_true(nrow(result) > 0)
  
  # Should have entries for multiple years
  unique_years <- unique(floor(result$time + min(crops$year)))
  expect_true(length(unique_years) >= 2)
})

# Test time calculation accuracy
test_that("rc_input_event_amendment calculates time correctly", {
  crops <- data.table(B_LU = "nl_101", year = 2020)
  amendment <- data.table(
    year = 2020,
    month = 6,  # June
    cin_tot = 1000,
    cin_hum = 100,
    cin_dpm = 300,
    cin_rpm = 600,
    fr_eoc_p = 15
  )
  
  result <- with_mock_rc_crops({
    rc_input_event_amendment(crops, amendment)
  })
  
  # Time should be calculated as year + month/12 - min(year)
  # For year 2020, month 6: 2020 + 6/12 - 2020 = 0.5
  expected_time <- 6/12
  expect_true(any(abs(result$time - expected_time) < 0.01))
})

# Test data.table ordering
test_that("rc_input_event_amendment returns properly ordered results", {
  crops <- data.table(
    B_LU = rep("nl_101", 2),
    year = c(2020, 2021)
  )
  
  amendment <- data.table(
    year = c(2021, 2020),  # Intentionally out of order
    cin_tot = c(1000, 1500),
    cin_hum = c(100, 150),
    cin_dpm = c(300, 450),
    cin_rpm = c(600, 900),
    fr_eoc_p = c(15, 15)
  )
  
  result <- with_mock_rc_cores({
    rc_input_event_amendment(crops, amendment)
  })
  
  # Results should be ordered by time
  expect_true(all(diff(result$time) >= 0))
})

# Test melt operation and output structure
test_that("rc_input_event_amendment output has correct melted structure", {
  crops <- create_test_crops()
  amendment <- create_test_amendment()
  
  result <- with_mock_rc_crops({
    rc_input_event_amendment(crops, amendment)
  })
  
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
  crops <- create_test_crops()
  
  # Test minimum values
  min_amendment <- data.table(
    year = 2020,
    cin_tot = 0,
    cin_hum = 0,
    cin_dpm = 0,
    cin_rpm = 0,
    fr_eoc_p = 0
  )
  
  result_min <- with_mock_rc_crops({
    rc_input_event_amendment(crops[1], min_amendment)
  })
  
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
  
  result_max <- with_mock_rc_crops({
    rc_input_event_amendment(crops[1], max_amendment)
  })
  
  expect_s3_class(result_max, "data.table")
  expect_true(all(result_max$value >= 0))
})