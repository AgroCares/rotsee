
test_that("rc_input_amendment runs correctly", {
  # 1. Setup
  # Create an input data.table with two amendments in the same year (2023)
  # but in different months (April and October).
  amendments_dt <- data.table(
    P_NAME = c("Compost", "Manure"),
    P_DATE_FERTILIZATION = c("2023-04-01", "2023-10-01"),
    P_C_OF = c(100, 80),       # Fraction carbon (g C/kg)
    P_HC = c(0.5, 0.4),        # Humification coefficient
    P_DOSE = c(10000, 15000)   # Dose in kg/ha
  )
  
  # 2. Execute
  result_dt <- rc_input_amendment(dt = amendments_dt)
  
  # 3. Assert
  # Check that the result is a data.table with 2 rows
  expect_s3_class(result_dt, "data.table")
  expect_equal(nrow(result_dt), 2)
 
  # Check that months are preserved
  expect_equal(result_dt$month, c(4, 10))
  
  # --- Manually calculate expected values for the first amendment (Compost) ---
  # P_DOSE = 10000, P_C_OF = 100, P_HC = 0.5
  fr_dpm_rpm_1 <- -2.174 * 0.5 + 2.02
  cin_tot_1 <- 10000 * 100/1000
  cin_hum_1 <- 0.02 * cin_tot_1
  cin_dpm_1 <- (1 - 0.02) * cin_tot_1 * fr_dpm_rpm_1 / (1 + fr_dpm_rpm_1)
  cin_rpm_1 <- (1 - 0.02) * cin_tot_1 - cin_dpm_1
  
  
  # Check calculated values for the first row (April amendment)
  expect_equal(result_dt[month == 4, cin_tot], cin_tot_1)
  expect_equal(result_dt[month == 4, cin_hum], cin_hum_1)
  expect_equal(result_dt[month == 4, cin_dpm], cin_dpm_1)
  expect_equal(result_dt[month == 4, cin_rpm], cin_rpm_1)
  
  # --- Manually calculate expected values for the second amendment (Manure) ---
  # P_DOSE = 15000, P_C_OF = 80, P_HC = 0.4
  fr_dpm_rpm_2 <- -2.174 * 0.4 + 2.02
  cin_tot_2 <- 15000 * 80/1000
  cin_hum_2 <- 0.02 * cin_tot_2
  cin_dpm_2 <- (1 - 0.02) * cin_tot_2 * fr_dpm_rpm_2 / (1 + fr_dpm_rpm_2)
  cin_rpm_2 <- (1 - 0.02) * cin_tot_2 - cin_dpm_2
  
  # Check calculated values for the second row (October amendment)
  expect_equal(result_dt[month == 10, cin_tot], cin_tot_2)
  expect_equal(result_dt[month == 10, cin_hum], cin_hum_2)
  expect_equal(result_dt[month == 10, cin_dpm], cin_dpm_2)
  expect_equal(result_dt[month == 10, cin_rpm], cin_rpm_2)
})


test_that("rc_input_amendment throws error for invalid inputs", {
  expect_error(rc_input_amendment(dt = NULL))
})

test_that("rc_input_amendment works with B_C_OF_INPUT only", {
  amendments_dt <- data.table(
    P_DATE_FERTILIZATION = c("2023-04-01", "2023-10-01"),
    P_HC = c(0.5, 0.4),
    B_C_OF_INPUT = c(1000, 1200)  # Directly provide carbon input
  )
  result_dt <- rc_input_amendment(amendments_dt)
  expect_s3_class(result_dt, "data.table")
  expect_equal(nrow(result_dt), 2)
  expect_equal(result_dt$cin_tot, c(1000, 1200))
})

test_that("rc_input_amendment works with mixed B_C_OF_INPUT and P_DOSE/P_C_OF", {
  amendments_dt <- data.table(
    P_DATE_FERTILIZATION = c("2023-04-01", "2023-10-01"),
    P_HC = c(0.5, 0.4),
    B_C_OF_INPUT = c(1000, NA),
    P_DOSE = c(NA, 15000),
    P_C_OF = c(NA, 80)
  )
  result_dt <- rc_input_amendment(amendments_dt)
  expect_s3_class(result_dt, "data.table")
  expect_equal(nrow(result_dt), 2)
  expect_equal(result_dt$cin_tot[1], 1000)
  expect_equal(result_dt$cin_tot[2], 15000 * 80 / 1000)
})


test_that("rc_input_amendment throws error for missing required columns", {
  # Missing P_HC
  amendments_dt <- data.table(
    P_DATE_FERTILIZATION = c("2023-04-01"),
    P_DOSE = c(10000),
    P_C_OF = c(100)
  )
  expect_error(rc_input_amendment(amendments_dt))
  
  # Missing P_DATE_FERTILIZATION
  amendments_dt <- data.table(
    P_HC = c(0.5),
    P_DOSE = c(10000),
    P_C_OF = c(100)
  )
  expect_error(rc_input_amendment(amendments_dt))
})


test_that("rc_input_amendment throws error for invalid values", {
  # Invalid P_HC (out of bounds)
  amendments_dt <- data.table(
    P_DATE_FERTILIZATION = c("2023-04-01"),
    P_HC = c(1.5),  # Should be <= 1
    P_DOSE = c(10000),
    P_C_OF = c(100)
  )
  expect_error(rc_input_amendment(amendments_dt))
  
  # Invalid P_DATE_FERTILIZATION (not a date)
  amendments_dt <- data.table(
    P_DATE_FERTILIZATION = c("not-a-date"),
    P_HC = c(0.5),
    P_DOSE = c(10000),
    P_C_OF = c(100)
  )
  expect_error(rc_input_amendment(amendments_dt))
})


test_that("rc_input_amendment handles edge cases for P_HC", {
  # P_HC >= 0.92 (fr_dpm_rpm = 0)
  amendments_dt <- data.table(
    P_DATE_FERTILIZATION = c("2023-04-01"),
    P_HC = c(0.95),
    P_DOSE = c(10000),
    P_C_OF = c(100)
  )
  
  result_dt <- rc_input_amendment(amendments_dt)
  expect_equal(result_dt$fr_dpm_rpm, 0)
  
  # P_HC = NA (should use default fr_dpm_rpm = 1.44)
  amendments_dt <- data.table(
    P_DATE_FERTILIZATION = c("2023-04-01"),
    P_HC = c(NA),
    P_DOSE = c(10000),
    P_C_OF = c(100)
  )
  result_dt <- rc_input_amendment(amendments_dt)
  expect_equal(result_dt$fr_dpm_rpm, 1.44)
})

test_that("rc_input_amendment throws error for empty data.table", {
  amendments_dt <- data.table()
  expect_error(rc_input_amendment(amendments_dt))
})
