library(data.table)

test_that("rc_update_weather returns default weather data when input is NULL", {
 
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-12-31")
  
  default_weather <- rc_update_weather(NULL,
                                       dt.time = dt.time)
  
  expect_s3_class(default_weather, "data.table")
  expect_equal(nrow(default_weather), 24)
  expect_equal(ncol(default_weather), 7)
  expect_equal(names(default_weather), c("year", "month", "W_TEMP_MEAN_MONTH", "W_PREC_SUM_MONTH", "W_ET_REF_MONTH", "W_ET_ACT_MONTH", "W_ET_REFACT"))
})

test_that("rc_update_weather validates input data table", {
  # Create a valid data table
  valid_dt <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_ACT_MONTH = rep(47, 12)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-12-31")
  
  # Test with valid data table
  expect_no_error(rc_update_weather(dt = valid_dt, dt.time = dt.time))
  
  # Test missing columns
  invalid_dt <- copy(valid_dt)[, W_ET_REF_MONTH := NULL]
  expect_no_error(rc_update_weather(invalid_dt, dt.time = dt.time)) # allowed if W_ET_ACT_MONTH is supplied
  
  invalid_dt <- copy(valid_dt)
  invalid_dt[, month := NULL]
  expect_error(rc_update_weather(invalid_dt, dt.time = dt.time), "missing elements") # month must be provided
  
  invalid_dt <- copy(valid_dt)
  invalid_dt[, `:=`(W_TEMP_MEAN_MONTH = NULL, W_PREC_SUM_MONTH = NULL)]
  expect_error(rc_update_weather(invalid_dt, dt.time = dt.time), "missing elements") # W_TEMP_MEAN_MONTH and W_PREC_SUM_MONTH must be provided
  
  # Test invalid month values
  invalid_dt <- copy(valid_dt)
  invalid_dt[, month := 0]
  expect_error(rc_update_weather(invalid_dt, dt.time), "month")
  
  # Test invalid temperature values
  invalid_dt <- copy(valid_dt)
  invalid_dt[, W_TEMP_MEAN_MONTH := -50]
  expect_error(rc_update_weather(invalid_dt, dt.time), "W_TEMP_MEAN_MONTH")
  
  # Test invalid precipitation values
  invalid_dt <- copy(valid_dt)
  invalid_dt[, W_PREC_SUM_MONTH := -10]
  expect_error(rc_update_weather(invalid_dt, dt.time), "W_PREC_SUM_MONTH")
  
  # Test both ET columns NULL (invalid)
  invalid_dt <- copy(valid_dt)
  invalid_dt[,`:=`(W_ET_REF_MONTH = NULL, W_ET_ACT_MONTH = NULL)]
  expect_error(rc_update_weather(invalid_dt, dt.time),
               "one of 'W_ET_REF_MONTH' or 'W_ET_ACT_MONTH'")
  
  # Test both ET columns NA (invalid)
  invalid_dt <- copy(valid_dt)
  invalid_dt[,W_ET_REF_MONTH := NA_real_][,W_ET_ACT_MONTH := NA_real_]
  expect_error(rc_update_weather(invalid_dt, dt.time),
               "should not contain NA", fixed = TRUE)
  
  # Test if not all 12 months are provided (invalid)
  invalid_dt <- copy(valid_dt)
  invalid_dt <- invalid_dt[month %in% c(1:6),]
  expect_error(rc_update_weather(invalid_dt, dt.time),
               "Must have at least 12 rows")
  
  # Test if ET_REF is too high (invalid)
  invalid_dt <- copy(valid_dt)
  invalid_dt[,W_ET_REF_MONTH := 20000] # too high
  expect_error(rc_update_weather(invalid_dt, dt.time),
               "W_ET_REF_MONTH", fixed = TRUE)
  
  # Test if ET_ACT is too high (invalid)
  invalid_dt <- copy(valid_dt)
  invalid_dt[, W_ET_ACT_MONTH := 20000]  # too high
  expect_error(rc_update_weather(invalid_dt, dt.time),
               "W_ET_ACT_MONTH", fixed = TRUE)
  
})

test_that("rc_update_weather handles input with year column", {
  dt.time <- rc_time_period("2022-01-01", "2022-12-31")
  
  valid_dt <- data.table(
    year = 2022,
    month = 1:12,
    W_TEMP_MEAN_MONTH = 10,
    W_PREC_SUM_MONTH = 50,
    W_ET_REF_MONTH = 40,
    W_ET_ACT_MONTH = 30
  )
  
  out <- rc_update_weather(valid_dt, dt.time)
  expect_s3_class(out, "data.table")
  expect_true(all(c("year", "month") %in% names(out)))
  expect_equal(nrow(out), nrow(dt.time))
  expect_false(anyNA(out$W_ET_REF_MONTH))
  
  # test if year range does not cover simulation period
  invalid_dt <- copy(valid_dt)
  invalid_dt[, year := 2020]
  expect_error(rc_update_weather(invalid_dt, dt.time),
               "must contain all months in the simulation window", fixed = FALSE)
  
})



test_that("rc_update_weather default weather replicates correctly for simulation years", {
  dt.time <- rc_time_period("2020-01-01", "2021-12-31")
  out <- rc_update_weather(NULL, dt.time)
  
  # should repeat pattern for each year in dt.time
  n_years <- length(unique(dt.time$year))
  expect_equal(nrow(out), 24)
  expect_equal(uniqueN(out$month), 12)
  expect_true(all(c("year", "month", "W_ET_REF_MONTH") %in% names(out)))
  expect_s3_class(out, "data.table")
})

test_that("rc_update_weather handles W_ET_REFACT parameter correctly", {
  # Test with W_ET_REFACT supplied
  weather_with_correction <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_ACT_MONTH = rep(NA_real_, 12),
    W_ET_REFACT = rep(0.8, 12)
  )
  
  dt.time <- rotsee::rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result <- rc_update_weather(weather_with_correction, dt.time = dt.time)
  
  expect_s3_class(result, "data.table")
  expect_true("W_ET_REFACT" %in% names(result))
  expect_equal(result$W_ET_REFACT, rep(0.8, 12))
})

test_that("rc_update_weather handles partials NAs in W_ET_REFACT", {
  # Test with partial NAs in W_ET_REFACT - should fill with 0.75
  weather_partial_na <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_REFACT = c(0.8, NA, 0.7, NA, rep(0.75, 8))
  )
  
  dt.time <- rotsee::rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result <- rc_update_weather(weather_partial_na, dt.time)
  expect_equal(result$W_ET_REFACT, c(0.8, 0.75, 0.7, 0.75, rep(0.75, 8)))
})

test_that("rc_update_weather runs without W_ET_REFACT column", {
  # Test without W_ET_REFACT column - should add default 0.75
  weather_no_correction <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12)
  )
  
  dt.time <- rotsee::rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result <- rc_update_weather(weather_no_correction, dt.time)
  expect_true("W_ET_REFACT" %in% names(result))
  expect_equal(result$W_ET_REFACT, rep(0.75, 12))
})

test_that("rc_update_weather validates W_ET_REFACT ranges", {
  # Test with out of range W_ET_REFACT values (too high)
  invalid_high <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_REFACT = rep(2.5, 12)
  )
  
  dt.time <- rotsee::rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  expect_error(rc_update_weather(invalid_high, dt.time), "W_ET_REFACT")
  
  # Test with negative W_ET_REFACT values
  invalid_negative <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_REFACT = rep(-0.1, 12)
  )
  
  expect_error(rc_update_weather(invalid_negative, dt.time), "W_ET_REFACT")
})

test_that("rc_update_weather boundary values for W_ET_REFACT", {
  # Test with W_ET_REFACT at lower boundary (0.3)
  weather_lower <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_REFACT = rep(0.3, 12)
  )
  
  dt.time <- rotsee::rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  expect_no_error(rc_update_weather(weather_lower, dt.time))
  
  # Test with W_ET_REFACT at upper boundary (2)
  weather_upper <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_REFACT = rep(2, 12)
  )
  
  dt.time <- rotsee::rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result_upper <- rc_update_weather(weather_upper, dt.time)
  expect_equal(result_upper$W_ET_REFACT, rep(2, 12))
  
  # Test with mixed W_ET_REFACT values
  weather_mixed <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_REFACT = seq(0.3, 2, length.out = 12)
  )
  
  dt.time <- rotsee::rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result_mixed <- rc_update_weather(weather_mixed, dt.time)
  expect_equal(result_mixed$W_ET_REFACT, seq(0.3, 2, length.out = 12), tolerance = 1e-10)
})

test_that("rc_update_weather default weather includes W_ET_REFACT", {
  # When no weather data is provided, default should include W_ET_REFACT
  dt.time <- rotsee::rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  default_weather <- rc_update_weather(NULL, dt.time)
  
  expect_true("W_ET_REFACT" %in% names(default_weather))
  expect_equal(default_weather$W_ET_REFACT, rep(0.75, 12))
  expect_equal(nrow(default_weather), 12)
  expect_equal(ncol(default_weather), 7)
})

test_that("rc_update_weather with only actual ET and W_ET_REFACT", {
  # Test scenario with only actual ET and W_ET_REFACT
  weather_actual_only <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_ACT_MONTH = rep(40, 12),
    W_ET_REFACT = rep(0.85, 12)
  )
  
  dt.time <- rotsee::rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result <- rc_update_weather(weather_actual_only, dt.time)
  expect_true("W_ET_REFACT" %in% names(result))
  expect_equal(result$W_ET_REFACT, rep(0.85, 12))
})

test_that("rc_update_weather edge case with both ET types and W_ET_REFACT", {
  # Both reference and actual ET provided with W_ET_REFACT
  weather_both <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_ACT_MONTH = c(rep(40, 6), rep(NA_real_, 6)),
    W_ET_REFACT = rep(0.8, 12)
  )
  
  dt.time <- rotsee::rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result <- rc_update_weather(weather_both, dt.time)
  expect_equal(result$W_ET_REFACT, rep(0.8, 12))
  expect_equal(nrow(result), 12)
})

test_that("rc_update_weather preserves other columns when adding W_ET_REFACT", {
  # Ensure no side effects on other columns
  weather_original <- data.table(
    month = 1:12,
    W_TEMP_MEAN_MONTH = c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2),
    W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
    W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5)
  )
  
  dt.time <- rotsee::rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result <- rc_update_weather(weather_original, dt.time)
  
  # Check original columns are preserved
  expect_equal(result$month, weather_original$month)
  expect_equal(result$W_TEMP_MEAN_MONTH, weather_original$W_TEMP_MEAN_MONTH)
  expect_equal(result$W_PREC_SUM_MONTH, weather_original$W_PREC_SUM_MONTH)
  expect_equal(result$W_ET_REF_MONTH, weather_original$W_ET_REF_MONTH)
})



test_that("rc_update_parms correctly runs when no parms supplied", {
  
  
  # Set default crop table
  crops <- data.table(crop = c(1, 2),
                      B_LU_START = c("2022-01-01", "2023-01-01"),
                      B_LU_END = c("2022-09-01", "2023-09-01"))
  
  result_crop <- rc_update_parms(crops = crops)
  
  expect_type(result_crop, "list")
  expect_equal(result_crop$dec_rates, c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02))
  expect_equal(result_crop$c_fractions, c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015))
  expect_true(result_crop$initialize)
  expect_equal(result_crop$unit, "A_SOM_LOI")
  expect_equal(result_crop$method, "adams")
  expect_equal(result_crop$poutput, "month")
  expect_equal(result_crop$start_date, min(as.Date(crops$B_LU_START)))
  expect_equal(result_crop$end_date, max(as.Date(crops$B_LU_END)))
})

test_that("rc_update_parms accepts and validates dec_rates", {
  # Set default crop table
  crops <- data.table(crop = c(1, 2),
                      B_LU_START = c("2022-01-01", "2023-01-01"),
                      B_LU_END = c("2022-09-01", "2023-09-01"))
  
  parms <- list(dec_rates = c(k1 = 5, k2 = 0.2, k3 = 0.5, k4 = 0.01))
  result <- rc_update_parms(parms, crops = crops)
  expect_equal(result$dec_rates, c(k1 = 5, k2 = 0.2, k3 = 0.5, k4 = 0.01))
  
  # Test partial dec_rates
  parms <- list(dec_rates = c(k1 = 5, k2 = 0.2))
  result <- rc_update_parms(parms, crops = crops)
  expect_equal(result$dec_rates, c(k1 = 5, k2 = 0.2, k3 = 0.66, k4 = 0.02))
  
  # Test invalid dec_rates
  expect_error(rc_update_parms(list(dec_rates = c(k1 = -1)), crops = crops), "not >= 0")
  expect_error(rc_update_parms(list(dec_rates = c(k1 = 31)), crops = crops), "not <= 30")
  expect_error(rc_update_parms(list(dec_rates = c(k5 = 1)), crops = crops), "has additional elements")
})

test_that("rc_update_parms accepts and validates c_fractions", {
  # Set default crop table
  crops <- data.table(crop = c(1, 2),
                      B_LU_START = c("2022-01-01", "2023-01-01"),
                      B_LU_END = c("2022-09-01", "2023-09-01"))
  
  parms <- list(c_fractions = c(fr_IOM = 0.05, fr_DPM = 0.02, fr_RPM = 0.1, fr_BIO = 0.02))
  result <- rc_update_parms(parms, crops = crops)
  expect_equal(result$c_fractions, c(fr_IOM = 0.05, fr_DPM = 0.02, fr_RPM = 0.1, fr_BIO = 0.02))
  
  # Test partial c_fractions
  parms <- list(c_fractions = c(fr_IOM = 0.05, fr_DPM = 0.02))
  result <- rc_update_parms(parms, crops = crops)
  expect_equal(result$c_fractions, c(fr_IOM = 0.05, fr_DPM = 0.02, fr_RPM = 0.125, fr_BIO = 0.015))
  
  # Test NA values are ignored (defaults kept)
  parms <- list(c_fractions = c(fr_IOM = NA_real_, fr_RPM = 0.2))
  result <- rc_update_parms(parms, crops = crops)
  expect_equal(result$c_fractions, c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.2, fr_BIO = 0.015))
  
  # Test invalid c_fractions
  expect_error(rc_update_parms(list(c_fractions = c(fr_IOM = -0.1)), crops = crops), "not >= 0")
  expect_error(rc_update_parms(list(c_fractions = c(fr_IOM = 1.1)), crops = crops), "not <= 1")
  expect_error(rc_update_parms(list(c_fractions = c(fr_X = 0.1)), crops = crops), "has additional elements")
})

test_that("rc_update_parms accepts and validates initialize", {
  # Set default crop table
  crops <- data.table(crop = c(1, 2),
                      B_LU_START = c("2022-01-01", "2023-01-01"),
                      B_LU_END = c("2022-09-01", "2023-09-01"))
  
  parms <- list(initialize = FALSE)
  result <- rc_update_parms(parms, crops = crops)
  expect_false(result$initialize)
  
  # Test invalid initialize
  expect_error(rc_update_parms(list(initialize = "TRUE")), "logical")
})

test_that("rc_update_parms accepts and validates start_date and end_date", {
  parms <- list(start_date = "2020-01-01", end_date = "2020-12-31")
  result <- rc_update_parms(parms)
  expect_equal(result$start_date, "2020-01-01")
  expect_equal(result$end_date, "2020-12-31")
  
  # Test invalid dates
  expect_error(rc_update_parms(list(start_date = "not-a-date")))
  expect_error(rc_update_parms(list(end_date = "not-a-date")))
  expect_error(rc_update_parms(list(start_date = "2020-12-31", end_date = "2020-01-01")), "Start_date is not before end_date")
})

test_that("rc_update_parms derives start_date and end_date from crops/amendments", {
  crops <- data.table(B_LU_START = c("2020-01-01", "2021-01-01"), B_LU_END = c("2020-12-31", "2021-12-31"))
  amendments <- data.table(P_DATE_FERTILIZATION = c("2020-06-01", "2021-06-01"))
  
  # Test with only crops
  result <- rc_update_parms(crops = crops)
  expect_equal(result$start_date, as.Date("2020-01-01"))
  expect_equal(result$end_date, as.Date("2021-12-31"))
  
  # Test with only amendments
  result <- rc_update_parms(amendments = amendments)
  expect_equal(result$start_date, as.Date("2020-06-01"))
  expect_equal(result$end_date, as.Date("2021-06-01"))
  
  # Test with both
  result <- rc_update_parms(crops = crops, amendments = amendments)
  expect_equal(result$start_date, as.Date("2020-01-01"))
  expect_equal(result$end_date, as.Date("2021-12-31"))
  
  # Test error if no dates found
  expect_error(rc_update_parms(), "No dates found in crops/amendments")
})

test_that("rc_update_parms accepts and validates unit", {
  # Set default crop table
  crops <- data.table(crop = c(1, 2),
                      B_LU_START = c("2022-01-01", "2023-01-01"),
                      B_LU_END = c("2022-09-01", "2023-09-01"))
  
  parms <- list(unit = "psoc")
  result <- rc_update_parms(parms, crops = crops)
  expect_equal(result$unit, "psoc")
  
  # Test invalid unit
  expect_error(rc_update_parms(list(unit = "invalid"), crops = crops), "additional elements")
})

test_that("rc_update_parms accepts and validates method", {
  # Set default crop table
  crops <- data.table(crop = c(1, 2),
                      B_LU_START = c("2022-01-01", "2023-01-01"),
                      B_LU_END = c("2022-09-01", "2023-09-01"))
  
  parms <- list(method = "adams")
  result <- rc_update_parms(parms, crops = crops)
  expect_equal(result$method, "adams")
  
  # Test invalid method
  expect_error(rc_update_parms(list(method = "invalid"), crops = crops), "element of set")
})

test_that("rc_update_parms accepts and validates poutput", {
  # Set default crop table
  crops <- data.table(crop = c(1, 2),
                      B_LU_START = c("2022-01-01", "2023-01-01"),
                      B_LU_END = c("2022-09-01", "2023-09-01"))
  
  parms <- list(poutput = "year")
  result <- rc_update_parms(parms, crops = crops)
  expect_equal(result$poutput, "year")
  
  # Test invalid poutput
  expect_error(rc_update_parms(list(poutput = "invalid"), crops = crops), "additional elements")
})

test_that("rc_check_inputs correctly validates soil_properties data", {
  # Generate valid soil properties data
  valid_soil <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  valid_crop <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)", "erwten (droog te oogsten)" ),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  valid_amendment <- data.table(
    P_ID = c(1, 1),
    P_NAME = c('cattle_slurry', 'cattle_slurry'),
    P_DOSE = c(63300, 63300),
    P_HC = c(0.7,0.7),
    P_C_OF = c(35, 35),
    P_DATE_FERTILIZATION = c("2022-05-01", "2023-05-01"))
  
  
  # Run with valid values (should not error)
  expect_no_error(rc_check_inputs(rothc_rotation = valid_crop,
                                  rothc_amendment = valid_amendment,
                                  soil_properties = valid_soil))
  
  # Run as a list (not allowed)
  soil_list <- as.list(valid_soil)
  expect_error(rc_check_inputs(rothc_rotation = valid_crop,
                  rothc_amendment = valid_amendment,
                  soil_properties = soil_list), "Must be a data.table")
  
  # Run without A_C_OF
  soil_no_ac <- copy(valid_soil)[, A_C_OF := NULL]
  expect_no_error(rc_check_inputs(rothc_rotation = valid_crop,
                  rothc_amendment = valid_amendment,
                  soil_properties = soil_no_ac))
  
  # Run without B_C_ST03
  soil_no_bc <- copy(valid_soil)[, B_C_ST03 := NULL]
  expect_no_error(rc_check_inputs(rothc_rotation = valid_crop,
                  rothc_amendment = valid_amendment,
                  soil_properties = soil_no_bc))

  # Run without A_C_OF and B_C_ST03
  soil_no_acbc <- copy(valid_soil)[, A_C_OF := NULL][, B_C_ST03 := NULL]
  expect_error(rc_check_inputs(rothc_rotation = valid_crop,
                  rothc_amendment = valid_amendment,
                  soil_properties = soil_no_acbc),
               "Both A_C_OF and B_C_ST03 are missing")
  
  # Run without clay
  soil_no_clay <- copy(valid_soil)[, A_CLAY_MI := NULL]
  expect_error(rc_check_inputs(rothc_rotation = valid_crop,
                  rothc_amendment = valid_amendment,
                  soil_properties = soil_no_clay),
               "Must be of type 'numeric'")
  
  # Run without bulk density
  soil_no_dens <- copy(valid_soil)[, A_DENSITY_SA := NULL]
  expect_error(rc_check_inputs(rothc_rotation = valid_crop,
                  rothc_amendment = valid_amendment,
                  soil_properties = soil_no_dens),
               "Must be of type 'numeric'")
})



test_that("rc_check_inputs correctly validates crop data", {
  # Generate valid soil properties data
  valid_soil <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  valid_crop <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01", "2024-04-01", "2025-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01", "2024-10-01", "2025-10-01"),
    B_LU = rep("nl_308", 4),
    B_LU_NAME = rep("erwten (droog te oogsten)", 4),
    B_LU_HC = rep(0.32, 4),
    B_C_OF_INPUT = rep(1500, 4)
  )
  
  valid_amendment <- data.table(
    P_ID = rep(1, 4),
    P_NAME = rep('cattle_slurry', 4),
    P_DOSE = rep(63300, 4),
    P_HC = rep(0.7,4),
    P_C_OF = rep(35, 4),
    P_DATE_FERTILIZATION = c("2022-05-01", "2023-05-01", "2024-05-01", "2025-05-01"))
  
 
  
  # Run as a list (not allowed)
  crop_list <- as.list(valid_crop)
  expect_error(rc_check_inputs(rothc_rotation = crop_list,
                  rothc_amendment = valid_amendment,
                  soil_properties = valid_soil), "Must be a data.table")
  
  # Without B_LU_START
  crop_no_start <- copy(valid_crop)[,B_LU_START := NULL]
  
  expect_error(rc_check_inputs(rothc_rotation = crop_no_start,
                  rothc_amendment = valid_amendment,
                  soil_properties = valid_soil),
               "missing elements")
  
  # Without B_LU_END
  crop_no_end <- copy(valid_crop)[,B_LU_END := NULL]
  
  expect_error(rc_check_inputs(rothc_rotation = crop_no_end,
                  rothc_amendment = valid_amendment,
                  soil_properties = valid_soil),
               "missing elements")
  
  # Without B_LU
  crop_no_lu <- copy(valid_crop)[,B_LU := NULL]
  
  expect_error(rc_check_inputs(rothc_rotation = crop_no_lu,
                  rothc_amendment = valid_amendment,
                  soil_properties = valid_soil),
               "missing elements")
  
  # Without B_LU_HC
  crop_no_hc <- copy(valid_crop)[,B_LU_HC := NULL]
  
  expect_error(rc_check_inputs(rothc_rotation = crop_no_hc,
                  rothc_amendment = valid_amendment,
                  soil_properties = valid_soil),
               "missing elements")
  
  # Without B_C_OF_INPUT
  crop_no_bc <- copy(valid_crop)[,B_C_OF_INPUT := NULL]
  
  expect_error(rc_check_inputs(rothc_rotation = crop_no_bc,
                  rothc_amendment = valid_amendment,
                  soil_properties = valid_soil),
               "missing elements")
  
})


test_that("rc_check_inputs correctly validates amendment data", {
  # Generate valid soil properties data
  valid_soil <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  valid_crop <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)", "erwten (droog te oogsten)" ),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  valid_amendment <- data.table(
    P_ID = c(1, 1),
    P_NAME = c('cattle_slurry', 'cattle_slurry'),
    P_DOSE = c(63300, 63300),
    P_HC = c(0.7,0.7),
    P_C_OF = c(35, 35),
    P_DATE_FERTILIZATION = c("2022-05-01", "2023-05-01"))
  
  
  
  # Run as a list (not allowed)
  amendment_list <- as.list(valid_amendment)
  expect_error(rc_check_inputs(rothc_rotation = valid_crop,
                  rothc_amendment = amendment_list,
                  soil_properties = valid_soil), "Must be a data.table")
  
  # run without HC
  amendment_no_hc <- copy(valid_amendment)[, P_HC := NULL]
  expect_error(rc_check_inputs(rothc_rotation = valid_crop,
                  rothc_amendment = amendment_no_hc,
                  soil_properties = valid_soil),
               "missing elements")
  
  # run without dose
  amendment_no_dose <- copy(valid_amendment)[, P_DOSE := NULL]
  rc_check_inputs(rothc_rotation = valid_crop,
                  rothc_amendment = amendment_no_dose,
                  soil_properties = valid_soil)
  
  amendment_no_pc <- copy(valid_amendment)[, P_C_OF := NULL]
  rc_check_inputs(rothc_rotation = valid_crop,
                  rothc_amendment = amendment_no_pc,
                  soil_properties = valid_soil)
  
  amendment_no_date <- copy(valid_amendment)[, P_DATE_FERTILIZATION := NULL]
  expect_error(rc_check_inputs(rothc_rotation = valid_crop,
                  rothc_amendment = amendment_no_date,
                  soil_properties = valid_soil),
               "missing elements")
})



test_that("rc_check_inputs handles edge case with maximum valid values", {
  # Create data with maximum valid values
  valid_soil_max <- data.table(
    A_C_OF = 200,  # Assuming high but valid
    B_C_ST03 = 500,  # Assuming high but valid
    A_CLAY_MI = 75,
    A_DENSITY_SA = 2.0
  )
  
  valid_crop <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)"),
    B_LU_HC = c(1.0),  # Maximum
    B_C_OF_INPUT = c(10000)  # High value
  )
  
  valid_amendment <- data.table(
    P_ID = c(1),
    P_NAME = c('cattle_slurry'),
    P_DOSE = c(100000),
    P_HC = c(1.0),  # Maximum
    P_C_OF = c(100),
    P_DATE_FERTILIZATION = c("2022-05-01")
  )
  
  # Should work with high but valid values
  expect_no_error(
    rc_check_inputs(rothc_rotation = valid_crop,
                    rothc_amendment = valid_amendment,
                    soil_properties = valid_soil_max)
  )
})

test_that("rc_check_inputs handles multiple amendment dates correctly", {
  valid_soil <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  valid_crop <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  # Multiple amendments at different times
  multi_amendment <- data.table(
    P_ID = c(1, 1, 2),
    P_NAME = c('cattle_slurry', 'cattle_slurry', 'compost'),
    P_DOSE = c(63300, 50000, 40000),
    P_HC = c(0.7, 0.7, 0.5),
    P_C_OF = c(35, 35, 45),
    P_DATE_FERTILIZATION = c("2022-03-01", "2022-05-01", "2022-07-15")
  )
  
  expect_no_error(
    rc_check_inputs(rothc_rotation = valid_crop,
                    rothc_amendment = multi_amendment,
                    soil_properties = valid_soil)
  )
})

test_that("rc_check_inputs validates date ordering in crops", {
  valid_soil <- data.table(
    A_C_OF = 50,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  # Crop with end before start (invalid dates)
  crop_bad_dates <- data.table(
    B_LU_START = c("2022-10-01"),  # Start after end
    B_LU_END = c("2022-04-01"),
    B_LU = c("nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  # rc_check_inputs validates that dates are valid Date objects yet ordering is not logical
  expect_error(
    rc_check_inputs(rothc_rotation = crop_bad_dates,
                    rothc_amendment = NULL,
                    soil_properties = valid_soil),
    "start date after end date in row: 1"
  )
  
  # rc_check_inputs validates that dates are valid Date objects yet ordering is not logical with multiple crops
  # wrong crop in row 2
  multi_crop_bad_dates <- data.table(
    B_LU_START = c("2022-04-01", "2023-10-01", "2024-04-01"),  # Start after end
    B_LU_END = c("2022-10-01", "2023-04-01", "2024-10-01"),
    B_LU = rep("nl_308", 3),
    B_LU_NAME = rep("erwten (droog te oogsten)", 3),
    B_LU_HC = rep(0.32, 3),
    B_C_OF_INPUT = rep(1500, 3)
  )
  
  expect_error(
    rc_check_inputs(rothc_rotation = multi_crop_bad_dates,
                    rothc_amendment = NULL,
                    soil_properties = valid_soil),
    "start date after end date in row: 2"
  )
  
  # wrong crop in rows 1 and 2
  multi_crop_bad_dates <- data.table(
    B_LU_START = c("2022-10-01", "2023-10-01", "2024-04-01"),  # Start after end
    B_LU_END = c("2022-04-01", "2023-04-01", "2024-10-01"),
    B_LU = rep("nl_308", 3),
    B_LU_NAME = rep("erwten (droog te oogsten)", 3),
    B_LU_HC = rep(0.32, 3),
    B_C_OF_INPUT = rep(1500, 3)
  )
  
  expect_error(
    rc_check_inputs(rothc_rotation = multi_crop_bad_dates,
                    rothc_amendment = NULL,
                    soil_properties = valid_soil),
    "start date after end date in row: 1, 2"
  )
})

test_that("rc_check_inputs handles missing optional P_NAME field", {
  valid_soil <- data.table(
    A_C_OF = 50,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  valid_crop <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_INPUT = c(1500)
  )
  
  # Amendment without P_NAME (optional field)
  amendment_no_name <- data.table(
    P_ID = c(1),
    P_DOSE = c(63300),
    P_HC = c(0.7),
    P_C_OF = c(35),
    P_DATE_FERTILIZATION = c("2022-05-01")
  )
  
  expect_no_error(
    rc_check_inputs(rothc_rotation = valid_crop,
                    rothc_amendment = amendment_no_name,
                    soil_properties = valid_soil)
  )
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



test_that("rc_set_refact correctly assigns W_ET_REFACT", {
  
  weather <- data.table(year = rep(2022:2023, each = 12),
    month = rep(1:12,2),
    W_TEMP_MEAN_MONTH = rep(c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2), 2),
    W_PREC_SUM_MONTH = rep(c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8), 2),
    W_ET_REF_MONTH = rep(c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5),2)
  )
  
  crop <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    D_MAKKINK_JAN = 0.36,
    D_MAKKINK_FEB = 0.36,
    D_MAKKINK_MAR = 0.36,
    D_MAKKINK_APR = 0.52,
    D_MAKKINK_MAY = 0.9,
    D_MAKKINK_JUN = 1.2,
    D_MAKKINK_JUL = 0.72,
    D_MAKKINK_AUG = 0.36,
    D_MAKKINK_SEP = 0.36,
    D_MAKKINK_OCT = 0.36,
    D_MAKKINK_NOV = 0.36,
    D_MAKKINK_DEC = 0.36
  )
  
  dt.time <- rc_time_period(start_date = "2022-04-01", end_date = "2023-10-01")
  
  expect_no_error(rc_set_refact(weather = weather, crop = crop, dt.time = dt.time))
  
})


test_that("rc_set_refact returns correct structure and values", {
    weather <- data.table(
        year = rep(2022:2023, each = 12),
        month = rep(1:12, 2),
        W_TEMP_MEAN_MONTH = rep(c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2), 2),
        W_PREC_SUM_MONTH = rep(c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8), 2),
        W_ET_REF_MONTH = rep(c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5), 2)
      )
    
      crop <- data.table(
          B_LU_START = c("2022-04-01", "2023-04-01"),
          B_LU_END = c("2022-10-01", "2023-10-01"),
          B_LU = c("nl_308", "nl_308"),
          D_MAKKINK_JAN = 0.36,
          D_MAKKINK_FEB = 0.36,
          D_MAKKINK_MAR = 0.36,
          D_MAKKINK_APR = 0.52,
          D_MAKKINK_MAY = 0.9,
          D_MAKKINK_JUN = 1.2,
          D_MAKKINK_JUL = 0.72,
          D_MAKKINK_AUG = 0.36,
          D_MAKKINK_SEP = 0.36,
          D_MAKKINK_OCT = 0.36,
          D_MAKKINK_NOV = 0.36,
          D_MAKKINK_DEC = 0.36
        )
      
        dt.time <- rc_time_period(start_date = "2022-04-01", end_date = "2023-10-01")
        
          result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
          
            # Check structure
            expect_s3_class(result, "data.table")
          expect_true("W_ET_REFACT" %in% names(result))
          expect_true(all(c("year", "month") %in% names(result)))
          
            # Check that result covers entire time period
            expect_equal(nrow(result), nrow(dt.time))
          
            # Check that default value (0.36) is applied outside crop growth period
            expect_true(all(result[month %in% c(1, 2, 3, 11, 12), W_ET_REFACT] == 0.36))
          
            # Check that crop-specific values are applied during growth period
            # April should have 0.52
            expect_equal(result[month == 4, unique(W_ET_REFACT)], 0.52)
          # May should have 0.9
            expect_equal(result[month == 5, unique(W_ET_REFACT)], 0.9)
          # June should have 1.2
            expect_equal(result[month == 6, unique(W_ET_REFACT)], 1.2)
          # July should have 0.72
            expect_equal(result[month == 7, unique(W_ET_REFACT)], 0.72)
        })

  test_that("rc_set_refact handles multiple crops correctly", {
      weather <- data.table(
          year = rep(2022:2023, each = 12),
          month = rep(1:12, 2),
          W_TEMP_MEAN_MONTH = rep(c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2), 2),
          W_PREC_SUM_MONTH = rep(c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8), 2),
          W_ET_REF_MONTH = rep(c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5), 2)
        )
      
        # Different crops with different Makkink factors
        crop <- data.table(
            B_LU_START = c("2022-04-01", "2022-11-01"),
            B_LU_END = c("2022-09-01", "2023-03-01"),
            B_LU = c("nl_308", "nl_259"),
            D_MAKKINK_JAN = c(0.36, 0.5),
            D_MAKKINK_FEB = c(0.36, 0.55),
            D_MAKKINK_MAR = c(0.36, 0.6),
            D_MAKKINK_APR = c(0.52, 0.4),
            D_MAKKINK_MAY = c(0.9, 0.4),
            D_MAKKINK_JUN = c(1.2, 0.4),
            D_MAKKINK_JUL = c(0.72, 0.4),
            D_MAKKINK_AUG = c(0.36, 0.4),
            D_MAKKINK_SEP = c(0.36, 0.4),
            D_MAKKINK_OCT = c(0.36, 0.4),
            D_MAKKINK_NOV = c(0.36, 0.45),
            D_MAKKINK_DEC = c(0.36, 0.48)
          )
        
          dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-12-31")
          
            result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
            
              expect_s3_class(result, "data.table")
            expect_true("W_ET_REFACT" %in% names(result))
            
              # Check that different crops have different factors in their respective periods
              # November 2022 should have 0.45 (from crop 2)
              expect_equal(result[year == 2022 & month == 11, W_ET_REFACT], 0.45)
            
              # January 2023 should have 0.5 (from crop 2)
              expect_equal(result[year == 2023 & month == 1, W_ET_REFACT], 0.5)
          })

  test_that("rc_set_refact handles single crop across years", {
      weather <- data.table(
          year = rep(2022:2024, each = 12),
          month = rep(1:12, 3),
          W_TEMP_MEAN_MONTH = rep(c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2), 3),
          W_PREC_SUM_MONTH = rep(c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8), 3),
          W_ET_REF_MONTH = rep(c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5), 3)
        )
      
        crop <- data.table(
            B_LU_START = "2022-04-01",
            B_LU_END = "2024-10-01",
            B_LU = "nl_308",
            D_MAKKINK_JAN = 0.36,
            D_MAKKINK_FEB = 0.36,
            D_MAKKINK_MAR = 0.36,
            D_MAKKINK_APR = 0.52,
            D_MAKKINK_MAY = 0.9,
            D_MAKKINK_JUN = 1.2,
            D_MAKKINK_JUL = 0.72,
            D_MAKKINK_AUG = 0.36,
            D_MAKKINK_SEP = 0.36,
            D_MAKKINK_OCT = 0.36,
            D_MAKKINK_NOV = 0.36,
            D_MAKKINK_DEC = 0.36
          )
        
          dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2024-12-31")
          
            result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
            
              expect_s3_class(result, "data.table")
            
              # Check continuity across years - May should have 0.9 in all years
              expect_true(all(result[month == 5, W_ET_REFACT] == 0.9))
            
              # Check months outside crop period in 2022 (Jan-Mar)
              expect_true(all(result[year == 2022 & month %in% 1:3, W_ET_REFACT] == 0.36))
            
              # Check months outside crop period in 2024 (Nov-Dec)
              expect_true(all(result[year == 2024 & month %in% 11:12, W_ET_REFACT] == 0.36))
          })

  test_that("rc_set_refact validates input parameters", {
      weather <- data.table(
          year = rep(2022:2023, each = 12),
          month = rep(1:12, 2),
          W_TEMP_MEAN_MONTH = rep(10, 24),
          W_PREC_SUM_MONTH = rep(50, 24),
          W_ET_REF_MONTH = rep(50, 24)
        )
      
        dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-12-31")
        
          # Missing required Makkink columns
          crop_incomplete <- data.table(
              B_LU_START = "2022-04-01",
              B_LU_END = "2022-10-01",
              B_LU = "nl_308",
              D_MAKKINK_JAN = 0.36,
              D_MAKKINK_FEB = 0.36
              # Missing other months
              )
          
            # Should error when Makkink columns are missing
            expect_error(
                rc_set_refact(weather = weather, crop = crop_incomplete, dt.time = dt.time),
                " Must be of type 'numeric'"
              )
        })

  test_that("rc_set_refact handles edge case dates correctly", {
      weather <- data.table(
          year = rep(2022, 12),
          month = 1:12,
          W_TEMP_MEAN_MONTH = c(3.6, 3.9, 6.5, 9.8, 13.4, 16.2, 18.3, 17.9, 14.7, 10.9, 7, 4.2),
          W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
          W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3, 6.5)
        )
      
        # Crop starting and ending on first day of month
        crop <- data.table(
            B_LU_START = "2022-05-01",
            B_LU_END = "2022-08-01",
            B_LU = "nl_308",
            D_MAKKINK_JAN = 0.36,
            D_MAKKINK_FEB = 0.36,
            D_MAKKINK_MAR = 0.36,
            D_MAKKINK_APR = 0.36,
            D_MAKKINK_MAY = 0.9,
            D_MAKKINK_JUN = 1.2,
            D_MAKKINK_JUL = 0.72,
            D_MAKKINK_AUG = 0.5,
            D_MAKKINK_SEP = 0.36,
            D_MAKKINK_OCT = 0.36,
            D_MAKKINK_NOV = 0.36,
            D_MAKKINK_DEC = 0.36
          )
        
          dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
          
            result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
            
              expect_s3_class(result, "data.table")
            
              # Verify correct months are covered
              expect_equal(result[month == 5, W_ET_REFACT], 0.9)
            expect_equal(result[month == 6, W_ET_REFACT], 1.2)
            expect_equal(result[month == 7, W_ET_REFACT], 0.72)
            expect_equal(result[month == 8, W_ET_REFACT], 0.5)
            
              # Months outside should have default
              expect_equal(result[month == 4, W_ET_REFACT], 0.36)
            expect_equal(result[month == 9, W_ET_REFACT], 0.36)
          })

  test_that("rc_set_refact handles overlapping crop periods", {
      weather <- data.table(
          year = rep(2022, 12),
          month = 1:12,
          W_TEMP_MEAN_MONTH = rep(10, 12),
          W_PREC_SUM_MONTH = rep(50, 12),
          W_ET_REF_MONTH = rep(50, 12)
        )
      
        # Overlapping crops (should use last one for overlapping periods)
        crop <- data.table(
            B_LU_START = c("2022-04-01", "2022-07-01"),
            B_LU_END = c("2022-09-01", "2022-10-01"),
            B_LU = c("nl_308", "nl_259"),
            D_MAKKINK_JAN = c(0.36, 0.36),
            D_MAKKINK_FEB = c(0.36, 0.36),
            D_MAKKINK_MAR = c(0.36, 0.36),
            D_MAKKINK_APR = c(0.52, 0.40),
            D_MAKKINK_MAY = c(0.9, 0.40),
            D_MAKKINK_JUN = c(1.2, 0.40),
            D_MAKKINK_JUL = c(0.72, 0.80),
            D_MAKKINK_AUG = c(0.9, 0.85),
            D_MAKKINK_SEP = c(0.36, 0.75),
            D_MAKKINK_OCT = c(0.36, 0.70),
            D_MAKKINK_NOV = c(0.36, 0.36),
            D_MAKKINK_DEC = c(0.36, 0.36)
          )
        
          dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
          
            result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
            
            expect_s3_class(result, "data.table")
            expect_true("W_ET_REFACT" %in% names(result))
            
            # Check that result covers entire time period
            expect_equal(nrow(result), nrow(dt.time))
            
            # July should have value from second crop (0.80) since it overlaps and it is the highest value
            expect_true(0.80 %in% result[month == 7, W_ET_REFACT])
            
            # August should have value from 1st crop (0.9) since crops overlap and it is the highest value
            expect_true(0.9 %in% result[month == 8, W_ET_REFACT])
          })

  test_that("rc_set_refact returns correct default for all non-crop months", {
      weather <- data.table(
          year = rep(2022, 12),
          month = 1:12,
          W_TEMP_MEAN_MONTH = rep(10, 12),
          W_PREC_SUM_MONTH = rep(50, 12),
          W_ET_REF_MONTH = rep(50, 12)
        )
      
        # Very short crop period
        crop <- data.table(
            B_LU_START = "2022-06-01",
            B_LU_END = "2022-07-01",
            B_LU = "nl_308",
            D_MAKKINK_JAN = 0.36,
            D_MAKKINK_FEB = 0.36,
            D_MAKKINK_MAR = 0.36,
            D_MAKKINK_APR = 0.36,
            D_MAKKINK_MAY = 0.36,
            D_MAKKINK_JUN = 1.2,
            D_MAKKINK_JUL = 0.72,
            D_MAKKINK_AUG = 0.36,
            D_MAKKINK_SEP = 0.36,
            D_MAKKINK_OCT = 0.36,
            D_MAKKINK_NOV = 0.36,
            D_MAKKINK_DEC = 0.36
          )
        
          dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
          
            result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
            
              # All months except June and July should have default 0.36
              expect_true(all(result[!month %in% c(6, 7), W_ET_REFACT] == 0.36))
            
              # June and July should have crop-specific values
              expect_equal(result[month == 6, W_ET_REFACT], 1.2)
            expect_equal(result[month == 7, W_ET_REFACT], 0.72)
          })

  test_that("rc_set_refact preserves data.table by reference semantics", {
      weather_original <- data.table(
          year = rep(2022, 12),
          month = 1:12,
          W_TEMP_MEAN_MONTH = rep(10, 12),
          W_PREC_SUM_MONTH = rep(50, 12),
          W_ET_REF_MONTH = rep(50, 12)
        )
      
        crop_original <- data.table(
            B_LU_START = "2022-04-01",
            B_LU_END = "2022-10-01",
            B_LU = "nl_308",
            D_MAKKINK_JAN = 0.36, D_MAKKINK_FEB = 0.36, D_MAKKINK_MAR = 0.36,
            D_MAKKINK_APR = 0.52, D_MAKKINK_MAY = 0.9, D_MAKKINK_JUN = 1.2,
            D_MAKKINK_JUL = 0.72, D_MAKKINK_AUG = 0.36, D_MAKKINK_SEP = 0.36,
            D_MAKKINK_OCT = 0.36, D_MAKKINK_NOV = 0.36, D_MAKKINK_DEC = 0.36
          )
        
          dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
          
            # Store original row counts
            weather_rows_before <- nrow(weather_original)
            crop_rows_before <- nrow(crop_original)
            
              result <- rc_set_refact(weather = weather_original, crop = crop_original, dt.time = dt.time)
              
                # Original data tables should remain unchanged (function uses copy())
                expect_equal(nrow(weather_original), weather_rows_before)
              expect_equal(nrow(crop_original), crop_rows_before)
              expect_false("W_ET_REFACT" %in% names(weather_original))
            })

  test_that("rc_set_refact handles year-spanning crop periods", {
      weather <- data.table(
          year = rep(2022:2023, each = 12),
          month = rep(1:12, 2),
          W_TEMP_MEAN_MONTH = rep(10, 24),
          W_PREC_SUM_MONTH = rep(50, 24),
          W_ET_REF_MONTH = rep(50, 24)
        )
      
        # Crop spanning from fall to spring
        crop <- data.table(
            B_LU_START = "2022-10-01",
            B_LU_END = "2023-04-01",
            B_LU = "nl_259",
            D_MAKKINK_JAN = 0.5,
            D_MAKKINK_FEB = 0.55,
            D_MAKKINK_MAR = 0.6,
            D_MAKKINK_APR = 0.65,
            D_MAKKINK_MAY = 0.4,
            D_MAKKINK_JUN = 0.4,
            D_MAKKINK_JUL = 0.4,
            D_MAKKINK_AUG = 0.4,
            D_MAKKINK_SEP = 0.4,
            D_MAKKINK_OCT = 0.45,
            D_MAKKINK_NOV = 0.48,
            D_MAKKINK_DEC = 0.52
          )
        
          dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-12-31")
          
            result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
            
              expect_s3_class(result, "data.table")
            
              # Check October 2022
              expect_equal(result[year == 2022 & month == 10, W_ET_REFACT], 0.45)
            
              # Check winter months 2022-2023
              expect_equal(result[year == 2022 & month == 11, W_ET_REFACT], 0.48)
            expect_equal(result[year == 2022 & month == 12, W_ET_REFACT], 0.52)
            expect_equal(result[year == 2023 & month == 1, W_ET_REFACT], 0.5)
            expect_equal(result[year == 2023 & month == 2, W_ET_REFACT], 0.55)
            
              # Check months outside crop period
              expect_equal(result[year == 2022 & month == 5, W_ET_REFACT], 0.36)
            expect_equal(result[year == 2023 & month == 6, W_ET_REFACT], 0.36)
          })

test_that("rc_set_refact handles single month crop period", {
  weather <- data.table(
    year = rep(2022, 12),
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12)
  )
  
  # Very short crop - only June
  crop <- data.table(
    B_LU_START = "2022-06-01",
    B_LU_END = "2022-06-30",
    B_LU = "nl_308",
    D_MAKKINK_JAN = 0.36,
    D_MAKKINK_FEB = 0.36,
    D_MAKKINK_MAR = 0.36,
    D_MAKKINK_APR = 0.36,
    D_MAKKINK_MAY = 0.36,
    D_MAKKINK_JUN = 1.5,
    D_MAKKINK_JUL = 0.36,
    D_MAKKINK_AUG = 0.36,
    D_MAKKINK_SEP = 0.36,
    D_MAKKINK_OCT = 0.36,
    D_MAKKINK_NOV = 0.36,
    D_MAKKINK_DEC = 0.36
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
  
  # Only June should have the crop-specific value
  expect_equal(result[month == 6, W_ET_REFACT], 1.5)
  expect_true(all(result[month != 6, W_ET_REFACT] == 0.36))
})

test_that("rc_set_refact removes existing W_ET_REFACT from weather", {
  # Weather already has W_ET_REFACT - should be replaced
  weather <- data.table(
    year = rep(2022, 12),
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12),
    W_ET_REFACT = rep(0.99, 12)  # This should be overwritten
  )
  
  crop <- data.table(
    B_LU_START = "2022-05-01",
    B_LU_END = "2022-07-01",
    B_LU = "nl_308",
    D_MAKKINK_JAN = 0.36, D_MAKKINK_FEB = 0.36, D_MAKKINK_MAR = 0.36,
    D_MAKKINK_APR = 0.36, D_MAKKINK_MAY = 0.8, D_MAKKINK_JUN = 1.0,
    D_MAKKINK_JUL = 0.7, D_MAKKINK_AUG = 0.36, D_MAKKINK_SEP = 0.36,
    D_MAKKINK_OCT = 0.36, D_MAKKINK_NOV = 0.36, D_MAKKINK_DEC = 0.36
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
  
  # Should NOT have 0.99 anymore
  expect_false(any(result$W_ET_REFACT == 0.99))
  expect_equal(result[month == 5, W_ET_REFACT], 0.8)
  expect_equal(result[month == 6, W_ET_REFACT], 1.0)
  expect_equal(result[month == 7, W_ET_REFACT], 0.7)
})

test_that("rc_set_refact handles crop with varying Makkink values", {
  weather <- data.table(
    year = rep(2022, 12),
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12)
  )
  
  # Crop with extreme variation in Makkink factors
  crop <- data.table(
    B_LU_START = "2022-01-01",
    B_LU_END = "2022-12-31",
    B_LU = "test_crop",
    D_MAKKINK_JAN = 0.3,   # Minimum
    D_MAKKINK_FEB = 0.5,
    D_MAKKINK_MAR = 0.7,
    D_MAKKINK_APR = 0.9,
    D_MAKKINK_MAY = 1.1,
    D_MAKKINK_JUN = 1.5,
    D_MAKKINK_JUL = 1.8,
    D_MAKKINK_AUG = 2.0,   # Maximum
    D_MAKKINK_SEP = 1.5,
    D_MAKKINK_OCT = 1.0,
    D_MAKKINK_NOV = 0.6,
    D_MAKKINK_DEC = 0.4
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
  
  # Verify all months get their specific values
  expect_equal(result[month == 1, W_ET_REFACT], 0.3)
  expect_equal(result[month == 6, W_ET_REFACT], 1.5)
  expect_equal(result[month == 8, W_ET_REFACT], 2.0)
  expect_equal(result[month == 12, W_ET_REFACT], 0.4)
})

test_that("rc_set_refact merges correctly with partial weather data", {
  # Weather data missing some months
  weather <- data.table(
    year = 2022,
    month = c(1, 3, 5, 7, 9, 11),  # Only odd months
    W_TEMP_MEAN_MONTH = rep(10, 6),
    W_PREC_SUM_MONTH = rep(50, 6),
    W_ET_REF_MONTH = rep(50, 6)
  )
  
  crop <- data.table(
    B_LU_START = "2022-04-01",
    B_LU_END = "2022-08-01",
    B_LU = "nl_308",
    D_MAKKINK_JAN = 0.36, D_MAKKINK_FEB = 0.36, D_MAKKINK_MAR = 0.36,
    D_MAKKINK_APR = 0.5, D_MAKKINK_MAY = 0.9, D_MAKKINK_JUN = 1.2,
    D_MAKKINK_JUL = 0.7, D_MAKKINK_AUG = 0.5, D_MAKKINK_SEP = 0.36,
    D_MAKKINK_OCT = 0.36, D_MAKKINK_NOV = 0.36, D_MAKKINK_DEC = 0.36
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
  
  # Should cover all 12 months in dt.time
  expect_equal(nrow(result), 12)
  expect_true("W_ET_REFACT" %in% names(result))
  
  # Crop months should have correct values
  expect_equal(result[month == 5, W_ET_REFACT], 0.9)
  expect_equal(result[month == 7, W_ET_REFACT], 0.7)
})

test_that("rc_set_refact handles successive crops correctly", {
  weather <- data.table(
    year = rep(2022, 12),
    month = 1:12,
    W_TEMP_MEAN_MONTH = rep(10, 12),
    W_PREC_SUM_MONTH = rep(50, 12),
    W_ET_REF_MONTH = rep(50, 12)
  )
  
  # Two successive crops with no gap
  crop <- data.table(
    B_LU_START = c("2022-03-01", "2022-07-01"),
    B_LU_END = c("2022-06-30", "2022-11-01"),
    B_LU = c("crop1", "crop2"),
    D_MAKKINK_JAN = c(0.36, 0.36),
    D_MAKKINK_FEB = c(0.36, 0.36),
    D_MAKKINK_MAR = c(0.6, 0.36),
    D_MAKKINK_APR = c(0.8, 0.36),
    D_MAKKINK_MAY = c(1.0, 0.36),
    D_MAKKINK_JUN = c(1.2, 0.36),
    D_MAKKINK_JUL = c(0.36, 0.7),
    D_MAKKINK_AUG = c(0.36, 0.9),
    D_MAKKINK_SEP = c(0.36, 1.1),
    D_MAKKINK_OCT = c(0.36, 1.3),
    D_MAKKINK_NOV = c(0.36, 0.8),
    D_MAKKINK_DEC = c(0.36, 0.36)
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
  
  # First crop period (Mar-Jun)
  expect_equal(result[month == 3, W_ET_REFACT], 0.6)
  expect_equal(result[month == 4, W_ET_REFACT], 0.8)
  
  # Second crop period (Jul-Nov)
  expect_equal(result[month == 8, W_ET_REFACT], 0.9)
  expect_equal(result[month == 10, W_ET_REFACT], 1.3)
  
  # Gap periods
  expect_equal(result[month == 1, W_ET_REFACT], 0.36)
  expect_equal(result[month == 12, W_ET_REFACT], 0.36)
})

test_that("rc_set_refact handles multi-year simulation with single crop", {
  weather <- data.table(
    year = rep(2022:2025, each = 12),
    month = rep(1:12, 4),
    W_TEMP_MEAN_MONTH = rep(10, 48),
    W_PREC_SUM_MONTH = rep(50, 48),
    W_ET_REF_MONTH = rep(50, 48)
  )
  
  # Long-duration crop spanning multiple years
  crop <- data.table(
    B_LU_START = "2023-01-01",
    B_LU_END = "2024-12-31",
    B_LU = "perennial",
    D_MAKKINK_JAN = 0.4, D_MAKKINK_FEB = 0.45, D_MAKKINK_MAR = 0.5,
    D_MAKKINK_APR = 0.6, D_MAKKINK_MAY = 0.8, D_MAKKINK_JUN = 1.0,
    D_MAKKINK_JUL = 1.2, D_MAKKINK_AUG = 1.1, D_MAKKINK_SEP = 0.9,
    D_MAKKINK_OCT = 0.7, D_MAKKINK_NOV = 0.5, D_MAKKINK_DEC = 0.4
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2025-12-31")
  
  result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
  
  expect_equal(nrow(result), 48)
  
  # 2022 should all be default
  expect_true(all(result[year == 2022, W_ET_REFACT] == 0.36))
  
  # 2023 and 2024 should have crop values
  expect_true(all(result[year == 2023 & month == 7, W_ET_REFACT] == 1.2))
  expect_true(all(result[year == 2024 & month == 5, W_ET_REFACT] == 0.8))
  
  # 2025 should be default
  expect_true(all(result[year == 2025, W_ET_REFACT] == 0.36))
})

test_that("rc_set_refact preserves all weather columns", {
  weather <- data.table(
    year = rep(2022, 12),
    month = 1:12,
    W_TEMP_MEAN_MONTH = seq(5, 16, length.out = 12),
    W_PREC_SUM_MONTH = seq(40, 90, length.out = 12),
    W_ET_REF_MONTH = seq(10, 100, length.out = 12),
    W_ET_ACT_MONTH = seq(8, 80, length.out = 12),
    custom_column = 1:12
  )
  
  crop <- data.table(
    B_LU_START = "2022-04-01",
    B_LU_END = "2022-09-01",
    B_LU = "nl_308",
    D_MAKKINK_JAN = 0.36, D_MAKKINK_FEB = 0.36, D_MAKKINK_MAR = 0.36,
    D_MAKKINK_APR = 0.5, D_MAKKINK_MAY = 0.9, D_MAKKINK_JUN = 1.2,
    D_MAKKINK_JUL = 0.7, D_MAKKINK_AUG = 0.5, D_MAKKINK_SEP = 0.4,
    D_MAKKINK_OCT = 0.36, D_MAKKINK_NOV = 0.36, D_MAKKINK_DEC = 0.36
  )
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2022-12-31")
  
  result <- rc_set_refact(weather = weather, crop = crop, dt.time = dt.time)
  
  # All original columns should be preserved
  expect_true(all(c("W_TEMP_MEAN_MONTH", "W_PREC_SUM_MONTH", "W_ET_REF_MONTH", 
                    "W_ET_ACT_MONTH", "custom_column") %in% names(result)))
  
  # Weather data should be unchanged
  expect_equal(result$W_TEMP_MEAN_MONTH, seq(5, 16, length.out = 12))
  expect_equal(result$custom_column, 1:12)
})



test_that("rc_visualize_plot runs without error", {
  # Use a small subset of your debug output
  dt <- data.table(
    time = 1:10,
    CDPM = rnorm(10, 100, 10),
    CRPM = rnorm(10, 200, 20),
    CBIO = rnorm(10, 300, 30),
    CHUM = rnorm(10, 400, 40),
    soc = rnorm(10, 1000, 100)
  )

  # Save current working directory and switch to temp directory
  old_wd <- getwd()
  temp_dir <- file.path(tempdir(), "rotsee_visualize_plot_test")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  setwd(temp_dir)
  
    # Restore working directory on exit (even if test fails)
    on.exit({
      setwd(old_wd)
      unlink(temp_dir, recursive = TRUE)
      }, add = TRUE)
    
  # Run the plot function
  expect_no_error(rc_visualize_plot(dt))
 
  # Check that debug files were created
  expect_true(file.exists("carbon_pools_linear.png"))
  expect_true(file.exists("carbon_pools_change.png"))
  
  # Check that the files are not empty
  expect_true(file.info(file.path("carbon_pools_linear.png"))$size > 0)
  expect_true(file.info(file.path("carbon_pools_change.png"))$size > 0)

  # Clean up
  file.remove("carbon_pools_linear.png")
  file.remove("carbon_pools_change.png")

  
 

})