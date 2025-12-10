source("helper-testdata.R")


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
  valid_dt <- create_weather()
  
  dt.time <- rc_time_period(start_date = "2022-01-01", end_date = "2023-12-31")
  
  # Test with valid data table
  expect_no_error(rc_update_weather(dt = valid_dt, dt.time = dt.time))
  
  # Test missing columns
  valid_dt_ref <- copy(valid_dt)[, W_ET_ACT_MONTH := NULL]
  expect_no_error(rc_update_weather(valid_dt_ref, dt.time = dt.time)) # only one of W_ET_REF_MONTH or W_ET_ACT_MONTH must be provided
  
  
  invalid_dt <- copy(valid_dt)[, W_ET_REF_MONTH := NULL]
  expect_error(rc_update_weather(invalid_dt, dt.time = dt.time), "missing values") # At least one of W_ET_REF_MONTH or W_ET_ACT_MONTH should not contain NAs
  
  invalid_dt <- copy(valid_dt)[, month := NULL]
  expect_error(rc_update_weather(invalid_dt, dt.time = dt.time), "missing elements") # month must be provided
  
  invalid_dt <- copy(valid_dt)[, `:=`(W_TEMP_MEAN_MONTH = NULL, W_PREC_SUM_MONTH = NULL)]
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
  
  valid_dt <- create_weather()[, year := 2022]
  
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

test_that("rc_update_parms correctly runs when no parms supplied", {

  
  # Set default crop table
  crops <- create_rotation()
  
  result_crop <- rc_update_parms(crops = crops)
  
  expect_type(result_crop, "list")
  expect_equal(result_crop$dec_rates, c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02))
  expect_equal(result_crop$c_fractions, c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015))
  expect_equal(result_crop$initialisation_method, "none")
  expect_equal(result_crop$unit, "A_SOM_LOI")
  expect_equal(result_crop$method, "adams")
  expect_equal(result_crop$poutput, "month")
  expect_equal(result_crop$start_date, min(as.Date(crops$B_LU_START)))
  expect_equal(result_crop$end_date, max(as.Date(crops$B_LU_END)))
})
  


test_that("rc_update_parms accepts and validates dec_rates", {
  # Set default crop table
  crops <- create_rotation()
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
  crops <- create_rotation()
  
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

test_that("rc_update_parms accepts and validates initialisation_method", {
  # Set default crop table
  crops <- create_rotation()
  
  
  # Test all four initialisation methods do not error
  methods <- c('spinup_analytical_bodemcoolstof', 'spinup_analytical_heuvelink', 'spinup_simulation', 'none')
  
  for (method in methods) {
    result <- rc_update_parms(parms =list(initialisation_method = method), crops = crops)
    
    expect_equal(result$initialisation_method, method)
  }

  # Test invalid initialisation_method errors
  expect_error(rc_update_parms(list(initialisation_method = "TRUE"), crops = crops), "element of set")
  
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
  crops <- create_rotation()
  amendments <- create_amendment()
  
  # Test with only crops
  result <- rc_update_parms(crops = crops)
  expect_equal(result$start_date, as.Date("2022-04-01"))
  expect_equal(result$end_date, as.Date("2023-10-01"))
  
  # Test with only amendments
  result <- rc_update_parms(amendments = amendments)
  expect_equal(result$start_date, as.Date("2022-05-01"))
  expect_equal(result$end_date, as.Date("2023-05-01"))
  
  # Test with both
  result <- rc_update_parms(crops = crops, amendments = amendments)
  expect_equal(result$start_date, as.Date("2022-04-01"))
  expect_equal(result$end_date, as.Date("2023-10-01"))
  
  # Test error if no dates found
  expect_error(rc_update_parms(), "No dates found in crops/amendments")
})

test_that("rc_update_parms accepts and validates unit", {
  # Set default crop table
  crops <- create_rotation()
  
  parms <- list(unit = "psoc")
  result <- rc_update_parms(parms, crops = crops)
  expect_equal(result$unit, "psoc")
  
  # Test invalid unit
  expect_error(rc_update_parms(list(unit = "invalid"), crops = crops), "element of set")
})

test_that("rc_update_parms accepts and validates method", {
  # Set default crop table
  crops <- create_rotation()
  
  parms <- list(method = "adams")
  result <- rc_update_parms(parms, crops = crops)
  expect_equal(result$method, "adams")
  
  # Test invalid method
  expect_error(rc_update_parms(list(method = "invalid"), crops = crops), "element of set")
})

test_that("rc_update_parms accepts and validates poutput", {
  # Set default crop table
  crops <- create_rotation()
  
  parms <- list(poutput = "year")
  result <- rc_update_parms(parms, crops = crops)
  expect_equal(result$poutput, "year")
  
  # Test invalid poutput
  expect_error(rc_update_parms(list(poutput = "invalid"), crops = crops), "element of set")
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



test_that("rc_calculate_bcof correctly validates input", {
  # Missing required column
  expect_error(
    rc_calculate_bcof(data.table(B_LU_YIELD = 30000, B_LU_HI = 0.6)),
    "must include"
  )
  
  # B_LU_YIELD out of bounds
  expect_error(
    rc_calculate_bcof(data.table(
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
    rc_calculate_bcof(data.table(
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
    rc_calculate_bcof(data.table(
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
    rc_calculate_bcof(data.table(
      B_LU_YIELD = 30000,
      B_LU_HI = 0.6,
      B_LU_HI_RES = 0.5,
      B_LU_RS_FR = -0.1,
      M_CROPRESIDUE = TRUE
    )),
    "is not >= 0"
  )
  
  # M_CROPRESIDUE not logical
  expect_error(
    rc_calculate_bcof(data.table(
      B_LU_YIELD = 30000,
      B_LU_HI = 0.6,
      B_LU_HI_RES = 0.5,
      B_LU_RS_FR = 1,
      M_CROPRESIDUE = "TRUE"
    )),
    "Must be of type 'logical'"
  )
})

test_that("rc_calculate_bcof correctly calculates C inputs", {
  # Set correct input data
  valid_dt <- data.table(
    B_LU_YIELD = 30000,
    B_LU_HI = 0.6,
    B_LU_HI_RES = 0.5,
    B_LU_RS_FR = 1,
    M_CROPRESIDUE = TRUE
  )
  
  # Run function with valid DT
  valid <- rc_calculate_bcof(valid_dt)
  
  # Check if everything went correctly
  expect_s3_class(valid, "data.table")
  expect_equal(valid$cin_aboveground, 30000 / 0.6 * 0.5, tolerance = 0.001)
  expect_equal(valid$cin_roots, valid$cin_aboveground * 1, tolerance = 0.001)
  expect_equal(valid$cin_residue, valid$cin_aboveground * 0.5, tolerance = 0.001)
  expect_equal(valid$B_C_OF_CULT, valid$cin_roots + valid$cin_residue, tolerance = 0.001)
})

test_that("rc_calculate_bcof handles edge cases", {
  # Zero yield
  zero_yield_dt <- data.table(
    B_LU_YIELD = 0,
    B_LU_HI = 0.6,
    B_LU_HI_RES = 0.5,
    B_LU_RS_FR = 1,
    M_CROPRESIDUE = TRUE
  )
  zero_yield <- rc_calculate_bcof(zero_yield_dt)
  expect_equal(zero_yield$cin_aboveground, 0)
  expect_equal(zero_yield$cin_roots, 0)
  expect_equal(zero_yield$cin_residue, 0)
  expect_equal(zero_yield$B_C_OF_CULT, 0)
  
  # No residue
  no_residue_dt <- data.table(
    B_LU_YIELD = 30000,
    B_LU_HI = 0.6,
    B_LU_HI_RES = 0.5,
    B_LU_RS_FR = 1,
    M_CROPRESIDUE = FALSE
  )
  no_residue <- rc_calculate_bcof(no_residue_dt)
  expect_equal(no_residue$cin_residue, 0)
  expect_equal(no_residue$B_C_OF_CULT, no_residue$cin_roots)
  
  # Max values
  max_dt <- data.table(
    B_LU_YIELD = 150000,
    B_LU_HI = 1,
    B_LU_HI_RES = 1,
    B_LU_RS_FR = 1,
    M_CROPRESIDUE = TRUE
  )
  max_result <- rc_calculate_bcof(max_dt)
  expect_equal(max_result$cin_aboveground, 150000 / 1 * 0.5, tolerance = 0.001)
  expect_equal(max_result$cin_roots, max_result$cin_aboveground, tolerance = 0.001)
  expect_equal(max_result$cin_residue, max_result$cin_aboveground * 1, tolerance = 0.001)
  expect_equal(max_result$B_C_OF_CULT, max_result$cin_roots + max_result$cin_residue, tolerance = 0.001)
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
    B_C_OF_CULT = 100
  )
  expect_error(rc_extend_crops(bad_crops, as.Date("2020-01-01")), "Element 1 is not <= ")
  
  # February 29th
  bad_crops <- data.table(
    B_LU_START = "2020-02-29",
    B_LU_END = "2020-12-31",
    B_LU = "Crop1",
    B_LU_HC = 0.5,
    B_C_OF_CULT = 100
  )
  expect_error(rc_extend_crops(bad_crops, as.Date("2020-01-01")), "February 29th")
  
  # Crop end date before start date
  bad_crops <- data.table(
    B_LU_START = "2020-12-31",
    B_LU_END = "2020-01-01",
    B_LU = "Crop1",
    B_LU_HC = 0.5,
    B_C_OF_CULT = 100
  )
  expect_error(rc_extend_crops(bad_crops, as.Date("2020-01-01"), simyears = 1), "Crop end date must be after crop start date")
  
  # Crop rotation plan outside simulation period
  bad_crops <- data.table(
    B_LU_START = "2019-01-01",
    B_LU_END = "2019-12-31",
    B_LU = "Crop1",
    B_LU_HC = 0.5,
    B_C_OF_CULT = 100
  )
  expect_error(rc_extend_crops(bad_crops, as.Date("2020-01-01"), simyears = 1), "crop rotation plan is outside of simulation period")
})

test_that("rc_extend_crops extends crops correctly with end_date", {
  crops <- data.table(
    B_LU_START = c("2020-01-01", "2020-06-01"),
    B_LU_END = c("2020-03-31", "2020-08-31"),
    B_LU = c("Crop1", "Crop2"),
    B_LU_HC = c(0.5, 0.3),
    B_C_OF_CULT = c(100, 200)
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
  expect_equal(names(result), c("B_LU_START", "B_LU_END", "B_LU", "B_LU_HC", "B_C_OF_CULT"))
})

test_that("rc_extend_crops extends crops correctly with simyears", {
  crops <- data.table(
    B_LU_START = c("2020-01-01", "2020-06-01"),
    B_LU_END = c("2020-03-31", "2020-08-31"),
    B_LU = c("Crop1", "Crop2"),
    B_LU_HC = c(0.5, 0.3),
    B_C_OF_CULT = c(100, 200)
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
  expect_equal(names(result), c("B_LU_START", "B_LU_END", "B_LU", "B_LU_HC", "B_C_OF_CULT"))
})

test_that("rc_extend_crops handles single year rotation", {
  crops <- data.table(
    B_LU_START = c("2020-01-01", "2020-06-01"),
    B_LU_END = c("2020-03-31", "2020-08-31"),
    B_LU = c("Crop1", "Crop2"),
    B_LU_HC = c(0.5, 0.3),
    B_C_OF_CULT = c(100, 200)
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
    B_C_OF_CULT = c(200, 100)
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
    B_C_OF_CULT = c(100, 200)
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



test_that("rc_update_parms validates c_fractions sum does not exceed 1", {
  # Set default crop table
  crops <- create_rotation()
  
  # Test fractions that sum to exactly 1
  parms <- list(c_fractions = c(fr_IOM = 0.25, fr_DPM = 0.25, fr_RPM = 0.25, fr_BIO = 0.5))
  expect_no_error(rc_update_parms(parms, crops = crops))
  
  # Test fractions that sum to less than 1
  parms <- list(c_fractions = c(fr_IOM = 0.2, fr_DPM = 0.3, fr_RPM = 0.2, fr_BIO = 0.2))
  expect_no_error(rc_update_parms(parms, crops = crops))
  
  # Test fractions that sum to more than 1 (should fail)
  parms <- list(c_fractions = c(fr_IOM = 0.3, fr_DPM = 0.8, fr_RPM = 0.3, fr_BIO = 0.3))
  expect_error(rc_update_parms(parms, crops = crops), "Sum of c_fractions.*exceeds 1")
  
  # Test fractions that barely exceed 1 (should fail)
  parms <- list(c_fractions = c(fr_IOM = 0.26, fr_DPM = 0.25, fr_RPM = 0.25, fr_BIO = 0.51))
  expect_error(rc_update_parms(parms, crops = crops), "Sum of c_fractions.*exceeds 1")
  
  # Test partial fractions where sum exceeds 1
  parms <- list(c_fractions = c(fr_IOM = 0.6, fr_DPM = 0.5, fr_RPM = 0.6))
  expect_error(rc_update_parms(parms, crops = crops), "Sum of c_fractions.*exceeds 1")
})

test_that("rc_update_parms validates c_fractions length", {
  crops <- create_rotation()
  
  # Test with 1 fraction (min allowed)
  parms <- list(c_fractions = c(fr_IOM = 0.05))
  expect_no_error(rc_update_parms(parms, crops = crops))
  
  # Test with 4 fractions (max allowed)
  parms <- list(c_fractions = c(fr_IOM = 0.05, fr_DPM = 0.02, fr_RPM = 0.1, fr_BIO = 0.02))
  expect_no_error(rc_update_parms(parms, crops = crops))
  
  # Test with empty vector (should fail)
  parms <- list(c_fractions = numeric(0))
  expect_error(rc_update_parms(parms, crops = crops), "Must have length >= 1")
})

test_that("rc_update_parms validates initialisation_method choices", {
  crops <- create_rotation()
  
  # Test valid choices
  valid_methods <- c('spinup_analytical_bodemcoolstof', 'spinup_analytical_heuvelink', 
                     'spinup_simulation', 'none')
  
  for (method in valid_methods) {
    parms <- list(initialisation_method = method)
    result <- rc_update_parms(parms, crops = crops)
    expect_equal(result$initialisation_method, method)
  }
  
  # Test invalid choice
  parms <- list(initialisation_method = 'invalid_method')
  expect_error(rc_update_parms(parms, crops = crops), "element of set")
  
  # Test non-character type
  parms <- list(initialisation_method = TRUE)
  expect_error(rc_update_parms(parms, crops = crops), "Must be of type 'character'")
  
  # Test multiple values
  parms <- list(initialisation_method = c('none', 'spinup_simulation'))
  expect_error(rc_update_parms(parms, crops = crops), "Must have length 1")
})

test_that("rc_update_parms validates unit parameter choices", {
  crops <- create_rotation()
  
  # Test valid unit choices
  valid_units <- c('A_SOM_LOI', 'psoc', 'cstock', 'psomperfraction')
  
  for (unit_val in valid_units) {
    parms <- list(unit = unit_val)
    result <- rc_update_parms(parms, crops = crops)
    expect_equal(result$unit, unit_val)
  }
  
  # Test invalid unit (omb was removed)
  parms <- list(unit = 'omb')
  expect_error(rc_update_parms(parms, crops = crops), "element of set")
  
  # Test case sensitivity - uppercase Cstock should fail
  parms <- list(unit = 'Cstock')
  expect_error(rc_update_parms(parms, crops = crops), "element of set")
  
  # Test multiple values
  parms <- list(unit = c('A_SOM_LOI', 'psoc'))
  expect_error(rc_update_parms(parms, crops = crops), "Must have length 1")
})

test_that("rc_update_parms returns default initialisation_method", {
  crops <- create_rotation()
  
  # Test default when no parms supplied
  result <- rc_update_parms(crops = crops)
  expect_equal(result$initialisation_method, 'none')
  
  # Test default when parms supplied but no initialisation_method
  parms <- list(unit = 'psoc')
  result <- rc_update_parms(parms, crops = crops)
  expect_equal(result$initialisation_method, 'none')
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

})