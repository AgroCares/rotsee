library(data.table)

test_that("rc_update_weather returns default weather data when input is NULL", {
  default_weather <- rc_update_weather(NULL,
                                       start_date = "2022-01-01",
                                       end_date = end_date <- "2024-01-01")
  expect_s3_class(default_weather, "data.table")
  expect_equal(nrow(default_weather), 36)
  expect_equal(ncol(default_weather), 6)
  expect_equal(names(default_weather), c("year", "month", "W_TEMP_MEAN_MONTH", "W_PREC_SUM_MONTH", "W_ET_POT_MONTH", "W_ET_ACT_MONTH"))
})

test_that("rc_update_weather validates input data table", {
  # Create a valid data table
  valid_dt <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_POT_MONTH = rep(50, 12),
    W_ET_ACT_MONTH = rep(47, 12)
  )
  
  start_date <- "2022-01-01"
  end_date <- "2024-01-01"
  # Test with valid data table
  expect_no_error(rc_update_weather(valid_dt, start_date, end_date))
  
  # Test missing columns
  invalid_dt <- valid_dt[, W_ET_POT_MONTH := NULL]
  expect_no_error(rc_update_weather(invalid_dt)) # only one of W_ET_POT_MONTH or W_ET_ACT_MONTH must be provided
  
  invalid_dt <- valid_dt[, month := NULL]
  expect_error(rc_update_weather(invalid_dt, start_date, end_date), "missing elements") # month must be provided
  
  invalid_dt <- valid_dt[, `:=`(W_TEMP_MEAN_MONTH = NULL, W_PREC_SUM_MONTH = NULL)]
  expect_error(rc_update_weather(invalid_dt, start_date, end_date), "missing elements") # W_TEMP_MEAN_MONTH and W_PREC_SUM_MONTH must be provided
  
  # Test invalid month values
  invalid_dt <- copy(valid_dt)
  invalid_dt[, month := 0]
  expect_error(rc_update_weather(invalid_dt, start_date, end_date), "month", fixed = FALSE)
  
  # Test invalid temperature values
  invalid_dt <- copy(valid_dt)
  invalid_dt[, W_TEMP_MEAN_MONTH := -50]
  expect_error(rc_update_weather(invalid_dt, start_date, end_date), "W_TEMP_MEAN_MONTH")
  
  # Test invalid precipitation values
  invalid_dt <- copy(valid_dt)
  invalid_dt[, W_PREC_SUM_MONTH := -10]
  expect_error(rc_update_weather(invalid_dt, start_date, end_date), "W_PREC_SUM_MONTH")
  
})


test_that("rc_calculate_bd correctly calculates bulk density",{
  dt <- data.table(A_CLAY_MI = 12,
                                A_SOM_LOI = 3)
  
  expect_no_error(rc_calculate_bd(dt = dt))
  
  # Test for highly organic soil
  high_OM_dt <- dt[,A_SOM_LOI:= 25]
    expect_no_error(rc_calculate_bd(dt = high_OM_dt))
  
  # Test with C concent as input
    C_dt <- dt[, A_SOM_LOI := NULL]
    C_dt <- dt[, A_C_OF := 80]
    expect_no_error(rc_calculate_bd(dt = C_dt))
})



test_that("rc_calculate_B_C_OF correctly validates input", {
  # Missing required column
  expect_error(
    rc_calculate_B_C_OF(data.table(B_LU_YIELD = 30000, B_LU_HI = 0.6)),
    "must include"
  )
  
  # B_LU_YIELD out of bounds
  expect_error(
    rc_calculate_B_C_OF(data.table(
      B_LU_YIELD = -1,
      B_LU_HI = 0.6,
      B_LU_HI_RES = 0.5,
      B_LU_RS_FR = 1,
      M_CROPRESIDUE = TRUE
    )),
    "is not >= 0"
  )
  
  # B_LU_HI out of bounds
  expect_error(
    rc_calculate_B_C_OF(data.table(
      B_LU_YIELD = 30000,
      B_LU_HI = 0,
      B_LU_HI_RES = 0.5,
      B_LU_RS_FR = 1,
      M_CROPRESIDUE = TRUE
    )),
    "is not >= 0.01"
  )
  
  # B_LU_HI_RES out of bounds
  expect_error(
    rc_calculate_B_C_OF(data.table(
      B_LU_YIELD = 30000,
      B_LU_HI = 0.6,
      B_LU_HI_RES = 1.1,
      B_LU_RS_FR = 1,
      M_CROPRESIDUE = TRUE
    )),
    "is not <= 1"
  )
  
  # B_LU_RS_FR out of bounds
  expect_error(
    rc_calculate_B_C_OF(data.table(
      B_LU_YIELD = 30000,
      B_LU_HI = 0.6,
      B_LU_HI_RES = 0.5,
      B_LU_RS_FR = 0,
      M_CROPRESIDUE = TRUE
    )),
    "is not >= 0.01"
  )
  
  # M_CROPRESIDUE not logical
  expect_error(
    rc_calculate_B_C_OF(data.table(
      B_LU_YIELD = 30000,
      B_LU_HI = 0.6,
      B_LU_HI_RES = 0.5,
      B_LU_RS_FR = 1,
      M_CROPRESIDUE = "TRUE"
    )),
    "Must be of type 'logical'"
  )
})

test_that("rc_calculate_B_C_OF correctly calculates C inputs", {
  # Set correct input data
  valid_dt <- data.table(
    B_LU_YIELD = 30000,
    B_LU_HI = 0.6,
    B_LU_HI_RES = 0.5,
    B_LU_RS_FR = 1,
    M_CROPRESIDUE = TRUE
  )
  
  # Run function with valid DT
  valid <- rc_calculate_B_C_OF(valid_dt)
  
  # Check if everything went correctly
  expect_s3_class(valid, "data.table")
  expect_equal(valid$cin_aboveground, 30000 / 0.6 * 0.5, tolerance = 0.001)
  expect_equal(valid$cin_roots, valid$cin_aboveground * 1, tolerance = 0.001)
  expect_equal(valid$cin_residue, valid$cin_aboveground * 0.5, tolerance = 0.001)
  expect_equal(valid$B_C_OF_INPUT, valid$cin_roots + valid$cin_residue, tolerance = 0.001)
})

test_that("rc_calculate_B_C_OF handles edge cases", {
  # Zero yield
  zero_yield_dt <- data.table(
    B_LU_YIELD = 0,
    B_LU_HI = 0.6,
    B_LU_HI_RES = 0.5,
    B_LU_RS_FR = 1,
    M_CROPRESIDUE = TRUE
  )
  zero_yield <- rc_calculate_B_C_OF(zero_yield_dt)
  expect_equal(zero_yield$cin_aboveground, 0)
  expect_equal(zero_yield$cin_roots, 0)
  expect_equal(zero_yield$cin_residue, 0)
  expect_equal(zero_yield$B_C_OF_INPUT, 0)
  
  # No residue
  no_residue_dt <- data.table(
    B_LU_YIELD = 30000,
    B_LU_HI = 0.6,
    B_LU_HI_RES = 0.5,
    B_LU_RS_FR = 1,
    M_CROPRESIDUE = FALSE
  )
  no_residue <- rc_calculate_B_C_OF(no_residue_dt)
  expect_equal(no_residue$cin_residue, 0)
  expect_equal(no_residue$B_C_OF_INPUT, no_residue$cin_roots)
  
  # Max values
  max_dt <- data.table(
    B_LU_YIELD = 150000,
    B_LU_HI = 1,
    B_LU_HI_RES = 1,
    B_LU_RS_FR = 5,
    M_CROPRESIDUE = TRUE
  )
  max_result <- rc_calculate_B_C_OF(max_dt)
  expect_equal(max_result$cin_aboveground, 150000 / 1 * 0.5, tolerance = 0.001)
  expect_equal(max_result$cin_roots, max_result$cin_aboveground * 5, tolerance = 0.001)
  expect_equal(max_result$cin_residue, max_result$cin_aboveground * 1, tolerance = 0.001)
  expect_equal(max_result$B_C_OF_INPUT, max_result$cin_roots + max_result$cin_residue, tolerance = 0.001)
})


test_that("rc_extend_crops validates inputs correctly", {
  # Empty data
  expect_error(rc_extend_crops(data.table(), as.Date("2020-01-01")), "Must have at least 1 row")
  
  # Missing required columns
  bad_crops <- data.table(B_LU_START = "2020-01-01", B_LU_END = "2020-12-31")
  expect_error(rc_extend_crops(bad_crops, as.Date("2020-01-01")), "must.include")
  
  # Invalid B_LU_HC
  bad_crops <- data.table(
    B_LU_START = "2020-01-01",
    B_LU_END = "2020-12-31",
    B_LU = "Crop1",
    B_LU_HC = 1.1,
    B_C_OF_INPUT = 100
  )
  expect_error(rc_extend_crops(bad_crops, as.Date("2020-01-01")), "Element 1 is not <= ")
  
  # February 29th
  bad_crops <- data.table(
    B_LU_START = "2020-02-29",
    B_LU_END = "2020-12-31",
    B_LU = "Crop1",
    B_LU_HC = 0.5,
    B_C_OF_INPUT = 100
  )
  expect_error(rc_extend_crops(bad_crops, as.Date("2020-01-01")), "February 29th")
  
  # Crop end date before start date
  bad_crops <- data.table(
    B_LU_START = "2020-12-31",
    B_LU_END = "2020-01-01",
    B_LU = "Crop1",
    B_LU_HC = 0.5,
    B_C_OF_INPUT = 100
  )
  expect_error(rc_extend_crops(bad_crops, as.Date("2020-01-01"), simyears = 1), "Crop end date must be after crop start date")
  
  # Crop rotation plan outside simulation period
  bad_crops <- data.table(
    B_LU_START = "2019-01-01",
    B_LU_END = "2019-12-31",
    B_LU = "Crop1",
    B_LU_HC = 0.5,
    B_C_OF_INPUT = 100
  )
  expect_error(rc_extend_crops(bad_crops, as.Date("2020-01-01"), simyears = 1), "crop rotation plan is outside of simulation period")
})

test_that("rc_extend_crops extends crops correctly with end_date", {
  crops <- data.table(
    B_LU_START = c("2020-01-01", "2020-06-01"),
    B_LU_END = c("2020-03-31", "2020-08-31"),
    B_LU = c("Crop1", "Crop2"),
    B_LU_HC = c(0.5, 0.3),
    B_C_OF_INPUT = c(100, 200)
  )
  
  result <- rc_extend_crops(crops, as.Date("2020-01-01"), as.Date("2022-12-31"))
  
  # Check number of rows
  expect_equal(nrow(result), 6)
  
  # Check dates are extended correctly
  expect_equal(result$B_LU_START, c(
    "2020-01-01", "2020-06-01",
    "2021-01-01", "2021-06-01",
    "2022-01-01", "2022-06-01"
  ))
  
  expect_equal(result$B_LU_END, c(
    "2020-03-31", "2020-08-31",
    "2021-03-31", "2021-08-31",
    "2022-03-31", "2022-08-31"
  ))
  
  # Check columns are preserved
  expect_equal(names(result), c("B_LU_START", "B_LU_END", "B_LU", "B_LU_HC", "B_C_OF_INPUT"))
})

test_that("rc_extend_crops extends crops correctly with simyears", {
  crops <- data.table(
    B_LU_START = c("2020-01-01", "2020-06-01"),
    B_LU_END = c("2020-03-31", "2020-08-31"),
    B_LU = c("Crop1", "Crop2"),
    B_LU_HC = c(0.5, 0.3),
    B_C_OF_INPUT = c(100, 200)
  )
  
  result <- rc_extend_crops(crops, as.Date("2020-01-01"), simyears = 3)
  
  # Check number of rows
  expect_equal(nrow(result), 6)
  
  # Check dates are extended correctly
  expect_equal(result$B_LU_START, c(
    "2020-01-01", "2020-06-01",
    "2021-01-01", "2021-06-01",
    "2022-01-01", "2022-06-01"
  ))
  
  expect_equal(result$B_LU_END, c(
    "2020-03-31", "2020-08-31",
    "2021-03-31", "2021-08-31",
    "2022-03-31", "2022-08-31"
  ))
  
  # Check columns are preserved
  expect_equal(names(result), c("B_LU_START", "B_LU_END", "B_LU", "B_LU_HC", "B_C_OF_INPUT"))
})

test_that("rc_extend_crops handles single year rotation", {
  crops <- data.table(
    B_LU_START = c("2020-01-01", "2020-06-01"),
    B_LU_END = c("2020-03-31", "2020-08-31"),
    B_LU = c("Crop1", "Crop2"),
    B_LU_HC = c(0.5, 0.3),
    B_C_OF_INPUT = c(100, 200)
  )
  
  result <- rc_extend_crops(crops, as.Date("2020-01-01"), simyears = 1)
  
  # Check number of rows
  expect_equal(nrow(result), 2)
  
  # Check dates are not extended
  expect_equal(result$B_LU_START, c("2020-01-01", "2020-06-01"))
  expect_equal(result$B_LU_END, c("2020-03-31", "2020-08-31"))
})

test_that("rc_extend_crops orders output by start date", {
  crops <- data.table(
    B_LU_START = c("2020-06-01", "2020-01-01"),
    B_LU_END = c("2020-08-31", "2020-03-31"),
    B_LU = c("Crop2", "Crop1"),
    B_LU_HC = c(0.3, 0.5),
    B_C_OF_INPUT = c(200, 100)
  )
  
  result <- rc_extend_crops(crops, as.Date("2020-01-01"), simyears = 1)
  
  # Check order
  expect_equal(result$B_LU_START, c("2020-01-01", "2020-06-01"))
})

test_that("rc_extend_crops removes temporary columns", {
  crops <- data.table(
    B_LU_START = c("2020-01-01", "2020-06-01"),
    B_LU_END = c("2020-03-31", "2020-08-31"),
    B_LU = c("Crop1", "Crop2"),
    B_LU_HC = c(0.5, 0.3),
    B_C_OF_INPUT = c(100, 200)
  )
  
  result <- rc_extend_crops(crops, as.Date("2020-01-01"), simyears = 1)
  
  # Check no temp columns
  expect_false(any(c("id", "year_start", "year_end", "yr_rep", "year_start_ext", "year_end_ext") %in% names(result)))
})


test_that("rc_extend_amendments validates inputs correctly", {
  # Empty data
  expect_error(rc_extend_amendments(data.table(), as.Date("2020-01-01")), "Must have at least 1 row")
  
  # Missing required columns
  bad_amendments <- data.table(P_HC = 0.5, P_DATE_FERTILIZATION = as.Date("2020-01-01"))
  names(bad_amendments) <- c("P_HC", "WRONG_NAME")
  expect_error(rc_extend_amendments(bad_amendments, as.Date("2020-01-01")), "must.include")
  
  # Invalid P_HC
  bad_amendments <- data.table(P_HC = 1.1, P_DATE_FERTILIZATION = as.Date("2020-01-01"))
  expect_error(rc_extend_amendments(bad_amendments, "2020-01-01"), "Element 1 is not <= 1")
  
  # February 29th
  bad_amendments <- data.table(P_HC = 0.5, P_DATE_FERTILIZATION = as.Date("2020-02-29"))
  expect_error(rc_extend_amendments(bad_amendments, "2020-01-01", end_date = "2030-01-01"), "February 29th")
  
  # Both end_date and simyears missing
  expect_error(rc_extend_amendments(bad_amendments, "2020-01-01", NULL, NULL), "both end_date and simyears are missing")
})

test_that("rc_extend_amendments extends amendments correctly with end_date", {
  amendments <- data.table(
    P_HC = c(0.5, 0.3),
    P_DATE_FERTILIZATION = as.Date(c("2020-01-01", "2020-06-01")),
    P_NAME = c("Amend1", "Amend2"),
    P_DOSE = c(100, 200)
  )
  
  result <- rc_extend_amendments(amendments, as.Date("2020-01-01"), as.Date("2022-12-31"))
  
  # Check number of rows
  expect_equal(nrow(result), 6)
  
  # Check dates are extended correctly
  expect_equal(result$P_DATE_FERTILIZATION, c(
    "2020-01-01", "2020-06-01",
    "2021-01-01", "2021-06-01",
    "2022-01-01", "2022-06-01"
  ))
  
  # Check columns are preserved
  expect_equal(names(result), c("P_HC", "P_DATE_FERTILIZATION", "P_NAME", "P_DOSE"))
})

test_that("rc_extend_amendments extends amendments correctly with simyears", {
  amendments <- data.table(
    P_HC = c(0.5, 0.3),
    P_DATE_FERTILIZATION = as.Date(c("2020-01-01", "2020-06-01")),
    P_NAME = c("Amend1", "Amend2"),
    P_DOSE = c(100, 200)
  )
  
  result <- rc_extend_amendments(amendments, as.Date("2020-01-01"), simyears = 3)
  
  # Check number of rows
  expect_equal(nrow(result), 6)
  
  # Check dates are extended correctly
  expect_equal(result$P_DATE_FERTILIZATION, c(
    "2020-01-01", "2020-06-01",
    "2021-01-01", "2021-06-01",
    "2022-01-01", "2022-06-01"
  ))
  
  # Check columns are preserved
  expect_equal(names(result), c("P_HC", "P_DATE_FERTILIZATION", "P_NAME", "P_DOSE"))
})

test_that("rc_extend_amendments handles single year rotation", {
  amendments <- data.table(
    P_HC = c(0.5, 0.3),
    P_DATE_FERTILIZATION = as.Date(c("2020-01-01", "2020-06-01")),
    P_NAME = c("Amend1", "Amend2"),
    P_DOSE = c(100, 200)
  )
  
  result <- rc_extend_amendments(amendments, as.Date("2020-01-01"), simyears = 1)
  
  # Check number of rows
  expect_equal(nrow(result), 2)
  
  # Check dates are not extended
  expect_equal(result$P_DATE_FERTILIZATION, c("2020-01-01", "2020-06-01"))
})

test_that("rc_extend_amendments orders output by date", {
  amendments <- data.table(
    P_HC = c(0.5, 0.3),
    P_DATE_FERTILIZATION = as.Date(c("2020-06-01", "2020-01-01")),
    P_NAME = c("Amend2", "Amend1"),
    P_DOSE = c(200, 100)
  )
  
  result <- rc_extend_amendments(amendments, as.Date("2020-01-01"), simyears = 1)
  
  # Check order
  expect_equal(result$P_DATE_FERTILIZATION, c("2020-01-01", "2020-06-01"))
})

test_that("rc_extend_amendments removes temporary columns", {
  amendments <- data.table(
    P_HC = c(0.5, 0.3),
    P_DATE_FERTILIZATION = as.Date(c("2020-01-01", "2020-06-01")),
    P_NAME = c("Amend1", "Amend2"),
    P_DOSE = c(100, 200)
  )
  
  result <- rc_extend_amendments(amendments, as.Date("2020-01-01"), simyears = 1)
  
  # Check no temp columns
  expect_false(any(c("id", "year", "yr_rep") %in% names(result)))
})


test_that("rc_time_period validates input correctly", {
  # Not a date
  expect_error(
    rc_time_period("not a date", "2025-12-31"),
    "format"
  )
  expect_error(
    rc_time_period("2025-01-01", "not a date"),
    "format"
  )
  
  # start_date after end_date
  expect_error(
    rc_time_period("2025-12-31", "2025-01-01"),
    "start_date must be on/before end_date"
  )
})

test_that("rc_time_period returns correct structure and values", {
  # Same year, same month
  same_month <- rc_time_period("2025-01-01", "2025-01-31")
  expect_s3_class(same_month, "data.table")
  expect_equal(nrow(same_month), 1)
  expect_equal(same_month$year, 2025)
  expect_equal(same_month$month, 1)
  expect_equal(same_month$time, 0, tolerance = 0.001)
  
  # Same year, different months
  same_year <- rc_time_period("2025-01-01", "2025-06-30")
  expect_s3_class(same_year, "data.table")
  expect_equal(nrow(same_year), 6)
  expect_equal(same_year$year, rep(2025, 6))
  expect_equal(same_year$month, 1:6)
  expect_equal(same_year$time, (1:6 - 1) / 12, tolerance = 0.001)
  
  # Different years, same month
  diff_year_same_month <- rc_time_period("2025-01-01", "2026-01-31")
  expect_s3_class(diff_year_same_month, "data.table")
  expect_equal(nrow(diff_year_same_month), 13)
  expect_equal(diff_year_same_month$year, c(rep(2025, 12), 2026))
  expect_equal(diff_year_same_month$month, c(1:12, 1))
  expect_equal(diff_year_same_month$time, (1:13 - 1) / 12, tolerance = 0.001)
  
  # Different years, different months
  diff_year_diff_month <- rc_time_period("2025-01-01", "2026-06-30")
  expect_s3_class(diff_year_diff_month, "data.table")
  expect_equal(nrow(diff_year_diff_month), 18)
  expect_equal(diff_year_diff_month$year, c(rep(2025, 12), rep(2026, 6)))
  expect_equal(diff_year_diff_month$month, c(1:12, 1:6))
  expect_equal(diff_year_diff_month$time, (1:18 - 1) / 12, tolerance = 0.001)
})

test_that("rc_time_period handles edge cases", {
  # Single day in month
  single_day <- rc_time_period("2025-01-15", "2025-01-15")
  expect_s3_class(single_day, "data.table")
  expect_equal(nrow(single_day), 1)
  expect_equal(single_day$year, 2025)
  expect_equal(single_day$month, 1)
  expect_equal(single_day$time, 0, tolerance = 0.001)
  
  # Leap year
  leap_year <- rc_time_period("2024-01-01", "2024-12-31")
  expect_s3_class(leap_year, "data.table")
  expect_equal(nrow(leap_year), 12)
  expect_equal(leap_year$year, rep(2024, 12))
  expect_equal(leap_year$month, 1:12)
  expect_equal(leap_year$time, (1:12 - 1) / 12, tolerance = 0.001)
  
  # Start and end on last day of month
  end_of_month <- rc_time_period("2025-01-31", "2025-02-28")
  expect_s3_class(end_of_month, "data.table")
  expect_equal(nrow(end_of_month), 2)
  expect_equal(end_of_month$year, rep(2025, 2))
  expect_equal(end_of_month$month, 1:2)
  expect_equal(end_of_month$time, (1:2 - 1) / 12, tolerance = 0.001)
})

