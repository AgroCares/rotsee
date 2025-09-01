library(testthat)
library(data.table)
# This test assumes the rotsee package is loaded, e.g. via devtools::load_all()

#test_that("rc_input_amendment processes multiple amendments in the same year", {
#  # 1. Setup
#  # Create an input data.table with two amendments in the same year (2023)
#  # but in different months (April and October).
#  amendments_dt <- data.table(
#    P_NAME = c("Compost", "Manure"),
#    year = c(2023, 2023),
#    month = c(4, 10),
#    P_C_OF = c(100, 80),       # Fraction carbon (g C/kg)
#    P_HC = c(0.5, 0.4),        # Humification coefficient
#    p_p2o5 = c(1.0, 1.2),      # Required but not used in C calculation
#    P_DOSE = c(10000, 15000)   # Dose in kg/ha
#  )
  
#  # 2. Execute
#  result_dt <- rc_input_amendment(dt = amendments_dt)
#  
#  # 3. Assert
#  # Check that the result is a data.table with 2 rows
#  expect_s3_class(result_dt, "data.table")
#  expect_equal(nrow(result_dt), 2)
#  
#  # Check that the year is normalized to 1
#  expect_true(all(result_dt$year == 1))
  
  # Check that months are preserved
#  expect_equal(result_dt$month, c(4, 10))
  
  # --- Manually calculate expected values for the first amendment (Compost) ---
  # P_DOSE = 10000, P_OM = 25, P_HC = 0.5
#  fr_dpm_rpm_1 <- -2.174 * 0.5 + 2.02
#  cin_tot_1 <- 10000 * 25 * 0.01 * 0.5
#  cin_hum_1 <- 0.02 * cin_tot_1
#  cin_dpm_1 <- (1 - 0.02) * cin_tot_1 * fr_dpm_rpm_1 / (1 + fr_dpm_rpm_1)
#  cin_rpm_1 <- (1 - 0.02) * cin_tot_1 - cin_dpm_1
  
  # Check calculated values for the first row (April amendment)
#  expect_equal(result_dt[month == 4, cin_tot], cin_tot_1)
#  expect_equal(result_dt[month == 4, cin_hum], cin_hum_1)
#  expect_equal(result_dt[month == 4, cin_dpm], cin_dpm_1)
#  expect_equal(result_dt[month == 4, cin_rpm], cin_rpm_1)
  
  # --- Manually calculate expected values for the second amendment (Manure) ---
  # P_DOSE = 15000, P_OM = 20, P_HC = 0.4
#  fr_dpm_rpm_2 <- -2.174 * 0.4 + 2.02
#  cin_tot_2 <- 15000 * 20 * 0.01 * 0.5
#  cin_hum_2 <- 0.02 * cin_tot_2
#  cin_dpm_2 <- (1 - 0.02) * cin_tot_2 * fr_dpm_rpm_2 / (1 + fr_dpm_rpm_2)
#  cin_rpm_2 <- (1 - 0.02) * cin_tot_2 - cin_dpm_2
  
  # Check calculated values for the second row (October amendment)
#  expect_equal(result_dt[month == 10, cin_tot], cin_tot_2)
#  expect_equal(result_dt[month == 10, cin_hum], cin_hum_2)
#  expect_equal(result_dt[month == 10, cin_dpm], cin_dpm_2)
#  expect_equal(result_dt[month == 10, cin_rpm], cin_rpm_2)
#})

#test_that("rc_input_amendment handles missing month column by adding NA", {
#  amendments_dt <- data.table(P_NAME = "Compost", year = 2023, P_C_OF = 100, P_HC = 0.5, p_p2o5 = 1, P_DOSE = 10000)
#  result_dt <- rc_input_amendment(dt = amendments_dt)
#  expect_true("month" %in% names(result_dt))
#  expect_true(is.na(result_dt$month))
#})

#test_that("rc_input_amendment throws error for invalid inputs", {
  # Both dt and B_LU_BRP are NULL, which should fail the assertion
#  expect_error(rc_input_amendment(dt = NULL, B_LU_BRP = NULL))
#})

