test_that("rc_input_crop validates input correctly", {
  # Mock data
  valid_dt <- data.table(
    B_LU_START = as.Date(c("2020-01-01", "2020-06-01")),
    B_LU_END = as.Date(c("2020-05-31", "2020-11-30")),
    B_LU = c("wheat", "maize"),
    B_LU_HC = c(0.8, 0.95),
    B_C_OF_INPUT = c(1000, 1500)
  )
  
  # Test: valid input
  expect_silent(rc_input_crop(valid_dt))
  
  # Test: missing required column
  invalid_dt <- copy(valid_dt[, .(B_LU_START, B_LU_END, B_LU, B_LU_HC)])
  expect_error(rc_input_crop(invalid_dt), "must.include")

  # Test: invalid B_C_OF_INPUT
  invalid_dt <- copy(valid_dt)
  invalid_dt[1, B_C_OF_INPUT := -100]
  expect_error(rc_input_crop(invalid_dt), "not >= 0")
})

test_that("rc_input_crop calculates derived columns correctly", {
  dt <- data.table(
    B_LU_START = as.Date(c("2020-01-01", "2020-06-01")),
    B_LU_END = as.Date(c("2020-05-31", "2020-11-30")),
    B_LU = c("wheat", "maize"),
    B_LU_HC = c(0.8, 0.95),
    B_C_OF_INPUT = c(1000, 1500)
  )
  
  result <- rc_input_crop(dt)

  # Check cin_dpm and cin_rpm
  expect_equal(result$cin_dpm[1], 1000 * (-2.174 * 0.8 + 2.02) / (1 + (-2.174 * 0.8 + 2.02)), tolerance = 0.001) # P_HC = 0.8
  expect_equal(result$cin_rpm[1], 1000 * 1 / (1 + (-2.174 * 0.8 + 2.02)), tolerance = 0.001)
  expect_equal(result$cin_dpm[2], 0, tolerance = 0.001) # P_HC > 0.92
  expect_equal(result$cin_rpm[2], 1500, tolerance = 0.001)
  
  # Check year and month
  expect_equal(result$year, c(2020, 2020))
  expect_equal(result$month, c(5, 11))
})

test_that("rc_input_crop handles missing B_LU_HC", {
  dt <- data.table(
    B_LU_START = as.Date(c("2020-01-01", "2020-06-01")),
    B_LU_END = as.Date(c("2020-05-31", "2020-11-30")),
    B_LU = c("wheat", "maize"),
    B_LU_HC = c(NA, 0.95),
    B_C_OF_INPUT = c(1000, 1500)
  )
  
  result <- rc_input_crop(dt)
  
  # Check default fr_dpm_rpm for NA HC
  expect_equal(result$cin_dpm[1], 1.44 * result$cin_rpm[1])
  expect_equal(result$cin_dpm[2], 0)
})

test_that("rc_input_crop returns only relevant columns", {
  dt <- data.table(
    B_LU_START = as.Date(c("2020-01-01", "2020-06-01")),
    B_LU_END = as.Date(c("2020-05-31", "2020-11-30")),
    B_LU = c("wheat", "maize"),
    B_LU_HC = c(0.8, 0.95),
    B_C_OF_INPUT = c(1000, 1500),
    extra_col = c("a", "b")
  )
  
  result <- rc_input_crop(dt)
  
  # Check only expected columns are returned
  expect_equal(
    names(result),
    c("year", "month", "B_LU_END", "B_LU_START", "cin_dpm", "cin_rpm")
  )
})

test_that("rc_input_crop works without B_LU column", {
  dt <- data.table(
    B_LU_START = as.Date(c("2020-01-01", "2020-06-01")),
    B_LU_END = as.Date(c("2020-05-31", "2020-11-30")),
    B_LU_HC = c(0.8, 0.95),
    B_C_OF_INPUT = c(1000, 1500)
  )
  
  # Should not error when B_LU is absent
  expect_silent(rc_input_crop(dt))
  
  result <- rc_input_crop(dt)
  
  # Verify output structure
  expect_equal(
    names(result),
    c("year", "month", "B_LU_END", "B_LU_START", "cin_dpm", "cin_rpm")
  )
})