library(data.table)

test_that("rc_update_weather returns default weather data when input is NULL", {
  default_weather <- rc_update_weather(NULL)
  expect_s3_class(default_weather, "data.table")
  expect_equal(nrow(default_weather), 12)
  expect_equal(ncol(default_weather), 6)
  expect_equal(names(default_weather), c("month", "W_TEMP_MEAN_MONTH", "W_PREC_SUM_MONTH", "W_ET_POT_MONTH", "W_ET_ACT_MONTH", "W_POT_TO_ACT"))
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
  
  # Test with valid data table
  expect_no_error(rc_update_weather(valid_dt))
  
  # Test missing columns
  invalid_dt <- valid_dt[, W_ET_POT_MONTH := NULL]
  expect_no_error(rc_update_weather(invalid_dt)) # only one of W_ET_POT_MONTH or W_ET_ACT_MONTH must be provided
  
  invalid_dt <- valid_dt[, month := NULL]
  expect_error(rc_update_weather(invalid_dt), "Must be a subset of") # month must be provided
  
  invalid_dt <- valid_dt[, `:=`(W_TEMP_MEAN_MONTH = NULL, W_PREC_SUM_MONTH = NULL)]
  expect_error(rc_update_weather(invalid_dt), "Must be a subset of") # W_TEMP_MEAN_MONTH and W_PREC_SUM_MONTH must be provided
  
  # Test invalid month values
  invalid_dt <- copy(valid_dt)
  invalid_dt[, month := 0]
  expect_error(rc_update_weather(invalid_dt), "Must be a subset of")
  
  # Test invalid temperature values
  invalid_dt <- copy(valid_dt)
  invalid_dt[, W_TEMP_MEAN_MONTH := -50]
  expect_error(rc_update_weather(invalid_dt), "W_TEMP_MEAN_MONTH")
  
  # Test invalid precipitation values
  invalid_dt <- copy(valid_dt)
  invalid_dt[, W_PREC_SUM_MONTH := -10]
  expect_error(rc_update_weather(invalid_dt), "W_PREC_SUM_MONTH")
  
  
})


test_that("rc_calculate_bd correctly calculates bulk density",{
  dt <- data.table(A_CLAY_MI = 12,
                                A_SOM_LOI = 3)
  
  expect_no_error(rc_calculate_bd(dt = dt))
  
  # Test for highly organic soil
  high_OM_dt <- dt[,A_SOM_LOI:= 25]
    expect_no_error(rc_calculate_bd(dt = dt))
  
  # Test with C concent as input
    C_dt <- dt[, A_SOM_LOI := NULL]
    C_dt <- dt[, A_C_OF := 80]
    expect_no_error(rc_calculate_bd(dt = dt))
})


test_that("rc_extend_crops correctly extends crop input file", {
  # Create a valid crop table
  crops <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)", "erwten (droog te oogsten)" ),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
  )
  
  # Define valid simyears
  simyears <- 50
  
  # Define valid start_date
  start_date <- "2022-04-01"
  
  # Check whether crop table is extended with valid data
  expect_no_error(rc_extend_crops(crops = crops, simyears = simyears, start_date = start_date))
})


test_that("rc_extend_amendments correctly extends amendments input file", {
  # Create a valid crop table
  amendments <- data.table(
    P_ID = c(1, 1),
    P_NAME = c('cattle_slurry', 'cattle_slurry'),
    P_DOSE = c(63300, 63300),
    P_HC = c(0.7,0.7),
    P_C_OF = c(35, 35),
    P_DATE_FERTILIZATION = c("2022-05-01", "2023-05-01"))
  
  # Define valid simyears
  simyears <- 50
  
  # Define valid start_date
  start_date <- "2022-04-01"
  
  # Check whether crop table is extended with valid data
  expect_no_error(rc_extend_amendments(amendments = amendments, simyears = simyears, start_date = start_date))
})

# context("cf_ind_importance")
#
# test_that("cf_ind_importance() works", {
