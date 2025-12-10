# test file for rc_initialise
# read in helper file
source("helper-testdata.R")

# conduct tests
test_that("rc_initialise runs properly", {
  # Create valid inputs
 # Set seed for reproducibility
  set.seed(123)

  # Create an event table with random C inputs
rothc.event <- data.table(
  time = rep(1:18, times = 3),
  var = rep(c("CDPM", "CRPM", "CHUM"), each = 18),
  method = "add",
  value = sample(500:2000, size = 54, replace = TRUE)
)

  dt.soc <- data.table(A_CLAY_MI = 18,
                     A_C_OF = 20,
                     toc = 210000)
  
dt.time <- rc_time_period(start_date = "2022-04-01", end_date = "2040-10-01")

start_date <- "2022-04-01"

crops <- create_rotation()

amendment <- create_amendment()

soil_properties <- create_soil_properties()

# Calculate rothc.parms
dt.weather <- create_weather()

dt.rmf <- rc_input_rmf(dt = crops,
                       B_DEPTH = 0.3,
                       A_CLAY_MI = dt.soc$A_CLAY_MI,
                       dt.weather = dt.weather,
                       dt.time = dt.time
                       )

rothc.parms <- list(k1 = 10,
                    k2 = 0.3,
                    k3=0.66,
                    k4=0.02,
                    R1 = dt.rmf$R1,
                    abcd = dt.rmf$abcd,
                    time = dt.rmf$time)

# Test all three methods do not error
methods <- c('spinup_analytical_bodemcoolstof', 'spinup_analytical_heuvelink', 'spinup_simulation')

for (method in methods) {
  expect_no_error(rc_initialise(crops = crops, amendment = amendment, dt.soc = dt.soc,
                          rothc.parms = rothc.parms, rothc.event = rothc.event,
                          dt.weather = dt.weather,
                          dt.time = dt.time, initialisation_method = method,
                          start_date = "2022-05-01", soil_properties = soil_properties))
}

})

test_that("rc_initialise validates required inputs for spinup_simulation", {
  set.seed(123)
  
  rothc.event <- data.table(
    time = rep(1:18, times = 3),
    var = rep(c("CDPM", "CRPM", "CHUM"), each = 18),
    method = "add",
    value = sample(500:2000, size = 54, replace = TRUE)
  )
  
  dt.soc <- data.table(A_CLAY_MI = 18, A_C_OF = 20, toc = 210000)
  dt.time <- rc_time_period(start_date = "2022-04-01", end_date = "2024-10-01")
  
  crops <- create_rotation()
  
  soil_properties <- create_soil_properties()
  
  dt.weather <- create_weather()
  
  dt.rmf <- rc_input_rmf(dt = crops, B_DEPTH = 0.3, A_CLAY_MI = dt.soc$A_CLAY_MI,
                         dt.weather = dt.weather, dt.time = dt.time)
  
  rothc.parms <- list(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02,
                      R1 = dt.rmf$R1, abcd = dt.rmf$abcd, time = dt.rmf$time)
  
  # Missing start_date for spinup_simulation
  expect_error(
    rc_initialise(crops = crops, dt.soc = dt.soc, rothc.parms = rothc.parms,
                  rothc.event = rothc.event, dt.time = dt.time,
                  soil_properties = soil_properties, dt.weather = dt.weather,
                  initialisation_method = 'spinup_simulation'),
    "start_date is required for spinup_simulation"
  )

  # Missing crops for spinup_simulation
  expect_error(
    rc_initialise(crops = NULL, dt.soc = dt.soc, rothc.parms = rothc.parms,
                  rothc.event = rothc.event, start_date = "2022-04-01",
                  dt.time = dt.time, soil_properties = soil_properties,
                  dt.weather = dt.weather, initialisation_method = 'spinup_simulation'),
    "crops is required for spinup_simulation"
  )
  
  # Missing soil_properties for spinup_simulation
  expect_error(
    rc_initialise(crops = crops, dt.soc = dt.soc, rothc.parms = rothc.parms,
                  rothc.event = rothc.event, start_date = "2022-04-01",
                  dt.time = dt.time, dt.weather = dt.weather,
                  initialisation_method = 'spinup_simulation'),
    "soil_properties is required for spinup_simulation"
  )
})

test_that("rc_initialise validates required inputs for analytical methods", {
  set.seed(123)
  
  rothc.event <- data.table(
    time = rep(1:18, times = 3),
    var = rep(c("CDPM", "CRPM", "CHUM"), each = 18),
    method = "add",
    value = sample(500:2000, size = 54, replace = TRUE)
  )
  
  dt.soc <- data.table(A_CLAY_MI = 18, A_C_OF = 20, toc = 210000)
  dt.time <- rc_time_period(start_date = "2022-04-01", end_date = "2024-10-01")
  
  crops <- create_rotation()
  
  dt.weather <- create_weather()
  
  dt.rmf <- rc_input_rmf(dt = crops, B_DEPTH = 0.3, A_CLAY_MI = dt.soc$A_CLAY_MI,
                         dt.time = dt.time, dt.weather = dt.weather)
  
  rothc.parms <- list(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02,
                      R1 = dt.rmf$R1, abcd = dt.rmf$abcd, time = dt.rmf$time)
  
  # Missing dt.time for analytical methods
  expect_error(
    rc_initialise(crops = crops, dt.soc = dt.soc, rothc.parms = rothc.parms,
                  rothc.event = rothc.event,
                  initialisation_method = 'spinup_analytical_heuvelink'),
    "dt.time is required for analytical spin-up types"
  )
  
  # Missing dt.soc
  expect_error(
    rc_initialise(crops = crops, rothc.parms = rothc.parms,
                  rothc.event = rothc.event, dt.time = dt.time,
                  initialisation_method = 'spinup_analytical_bodemcoolstof'),
    "dt.soc is required for analytical spin-up types"
  )
  
  # Missing A_CLAY_MI column
  dt.soc_bad <- data.table(toc = 210000)
  expect_error(
    rc_initialise(crops = crops, dt.soc = dt.soc_bad, rothc.parms = rothc.parms,
                  rothc.event = rothc.event, dt.time = dt.time,
                  initialisation_method = 'spinup_analytical_heuvelink'),
    "dt.soc must contain A_CLAY_MI column"
  )
  
  # Missing toc column
  dt.soc_bad <- data.table(A_CLAY_MI = 18)
  expect_error(
    rc_initialise(crops = crops, dt.soc = dt.soc_bad, rothc.parms = rothc.parms,
                  rothc.event = rothc.event, dt.time = dt.time,
                  initialisation_method = 'spinup_analytical_bodemcoolstof'),
    "dt.soc must contain toc column"
  )
})

test_that("rc_initialise handles NULL crops and amendments correctly", {
  set.seed(123)
  
  rothc.event <- data.table(
    time = rep(1:3, times = 3),
    var = rep(c("CDPM", "CRPM", "CHUM"), each = 3),
    method = "add",
    value = sample(500:2000, size = 9, replace = TRUE)
  )
  
  crops <- create_rotation()
  
  dt.weather <- create_weather()
  
  dt.soc <- data.table(A_CLAY_MI = 18, toc = 210000)
  dt.time <- rc_time_period(start_date = "2022-04-01", end_date = "2024-10-01")
  
  dt.rmf <- rc_input_rmf(dt = crops, B_DEPTH = 0.3, A_CLAY_MI = dt.soc$A_CLAY_MI,
                         dt.time = dt.time, dt.weather = dt.weather)
  
  rothc.parms <- list(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02,
                      R1 = dt.rmf$R1, abcd = dt.rmf$abcd, time = dt.rmf$time)
  
  # Test with NULL crops for analytical_heuvelink
  result <- rc_initialise(crops = NULL, amendment = NULL, dt.soc = dt.soc,
                          rothc.parms = rothc.parms, rothc.event = rothc.event,
                          dt.time = dt.time,
                          initialisation_method = 'spinup_analytical_heuvelink')
  

  expect_type(result, "double")
  expect_named(result, c("CIOM0", "CDPM0", "CRPM0", "CBIO0"))
  expect_true(all(result >= 0))
  expect_true(all(result <= dt.soc[,toc/1000]))
})

test_that("rc_initialise returns valid fraction structure", {
  set.seed(123)
  
  rothc.event <- data.table(
    time = rep(1:6, times = 3),
    var = rep(c("CDPM", "CRPM", "CHUM"), each = 6),
    method = "add",
    value = sample(500:2000, size = 18, replace = TRUE)
  )
  
  dt.soc <- data.table(A_CLAY_MI = 18, toc = 210000)
  dt.time <- rc_time_period(start_date = "2022-04-01", end_date = "2027-10-01")
  
  crops <- create_rotation()
  
  amendment <- create_amendment()
  
  dt.weather <- create_weather()
  
  soil_properties <- create_soil_properties()
  
  dt.rmf <- rc_input_rmf(dt = crops, B_DEPTH = 0.3, A_CLAY_MI = dt.soc$A_CLAY_MI,
                         dt.time = dt.time, dt.weather = dt.weather)
  
  rothc.parms <- list(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02,
                      R1 = dt.rmf$R1, abcd = dt.rmf$abcd, time = dt.rmf$time)
  
  # Test all three methods return valid structure
  methods <- c('spinup_analytical_bodemcoolstof', 'spinup_analytical_heuvelink', 'spinup_simulation')
  
  for (method in methods) {
    result <- rc_initialise(crops = crops, amendment = amendment, dt.soc = dt.soc,
                            rothc.parms = rothc.parms, rothc.event = rothc.event,
                            dt.time = dt.time, initialisation_method = method,
                            start_date = "2022-05-01", soil_properties = soil_properties)
    
    # Check structure
    expect_type(result, "double")
    expect_named(result, c("CIOM0", "CDPM0", "CRPM0", "CBIO0"))
    
    # Check values are valid fractions
    expect_true(all(result >= 0))
    expect_true(all(result <= dt.soc[,toc/1000]))
    expect_true(all(is.finite(result)))
    
    # Check sum is less than or equal to total C stock
    expect_lte(sum(result), dt.soc[,toc/1000])
  }
})

test_that("rc_initialise handles edge case with zero C inputs", {
  set.seed(123)
  
  rothc.event <- data.table(
    time = numeric(0), var = character(0), method = character(0), value = numeric(0)
  )
  
  dt.soc <- data.table(A_CLAY_MI = 18, toc = 210000)
  dt.time <- rc_time_period(start_date = "2022-04-01", end_date = "2024-10-01")
  
  # Crops with zero input
  crops <- data.table(
    B_LU_START = c("2022-04-01"),
    B_LU_END = c("2022-10-01"),
    B_LU = c("nl_308"),
    B_LU_HC = c(0.32),
    B_C_OF_CULT = c(0)
  )
  
  dt.weather <- create_weather()
  
  dt.rmf <- rc_input_rmf(dt = crops, B_DEPTH = 0.3, A_CLAY_MI = dt.soc$A_CLAY_MI,
                         dt.time = dt.time, dt.weather = dt.weather)
  
  rothc.parms <- list(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02,
                      R1 = dt.rmf$R1, abcd = dt.rmf$abcd, time = dt.rmf$time)
  
  # Should handle zero inputs gracefully
  result <- rc_initialise(crops = crops, amendment = NULL, dt.soc = dt.soc,
                          rothc.parms = rothc.parms, rothc.event = rothc.event,
                          dt.time = dt.time,
                          initialisation_method = 'spinup_analytical_heuvelink')
  
  expect_type(result, "double")
  expect_true(all(is.finite(result)))
})

test_that("rc_initialise spinup_analytical_heuvelink handles singular matrix", {
  set.seed(123)
  
  rothc.event <- data.table(
    time = rep(1:3, times = 3),
    var = rep(c("CDPM", "CRPM", "CHUM"), each = 3),
    method = "add",
    value = sample(500:2000, size = 9, replace = TRUE)
  )
  
  dt.soc <- data.table(A_CLAY_MI = 18, toc = 210000)
  dt.time <- rc_time_period(start_date = "2022-04-01", end_date = "2024-10-01")
  
  crops <- create_rotation()
  
  dt.weather <- create_weather()
  
  dt.rmf <- rc_input_rmf(dt = crops, B_DEPTH = 0.3, A_CLAY_MI = dt.soc$A_CLAY_MI,
                         dt.time = dt.time, dt.weather = dt.weather)
  
  # Create rothc.parms with all decomposition rates set to 0 (singular matrix)
  rothc.parms <- list(k1 = 0, k2 = 0, k3 = 0, k4 = 0,
                      R1 = dt.rmf$R1, abcd = dt.rmf$abcd, time = dt.rmf$time)
  
  # Should error with informative message
  expect_error(
    rc_initialise(crops = crops, dt.soc = dt.soc, rothc.parms = rothc.parms,
                  rothc.event = rothc.event, dt.time = dt.time,
                  initialisation_method = 'spinup_analytical_heuvelink'),
    "Matrix A4 is singular"
  )
})

test_that("rc_initialise bodemcoolstof handles negative biohum", {
  set.seed(456)
  
  # Create event with very high DPM and RPM inputs
  rothc.event <- data.table(
    time = rep(1:3, times = 3),
    var = rep(c("CDPM", "CRPM", "CHUM"), each = 3),
    method = "add",
    value = c(rep(50000, 3), rep(50000, 3), rep(100, 3))  # Very high DPM and RPM
  )
  
  dt.soc <- data.table(A_CLAY_MI = 18, toc = 50000)  # Low total C
  dt.time <- rc_time_period(start_date = "2022-04-01", end_date = "2024-10-01")
  
  crops <- create_rotation()
  
  dt.weather <- create_weather()
  
  dt.rmf <- rc_input_rmf(dt = crops, B_DEPTH = 0.3, A_CLAY_MI = dt.soc$A_CLAY_MI,
                         dt.time = dt.time, dt.weather = dt.weather)
  
  rothc.parms <- list(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02,
                      R1 = dt.rmf$R1, abcd = dt.rmf$abcd, time = dt.rmf$time)
  
  # Should handle this by using defaults
  result <- rc_initialise(crops = crops, dt.soc = dt.soc, rothc.parms = rothc.parms,
                          rothc.event = rothc.event, dt.time = dt.time,
                          initialisation_method = 'spinup_analytical_bodemcoolstof')
  
  expect_type(result, "double")
  expect_true(all(result >= 0))
  expect_true(all(is.finite(result)))
})

test_that("rc_initialise handles amendments with B_C_OF_AMENDMENT vs P_DOSE*P_C_OF", {
  set.seed(123)
  
  rothc.event <- data.table(
    time = rep(1:3, times = 3),
    var = rep(c("CDPM", "CRPM", "CHUM"), each = 3),
    method = "add",
    value = sample(500:2000, size = 9, replace = TRUE)
  )
  
  dt.soc <- data.table(A_CLAY_MI = 18, toc = 210000)
  dt.time <- rc_time_period(start_date = "2022-04-01", end_date = "2024-10-01")
  
  crops <- create_rotation()
  
  dt.weather <- create_weather()
  
  dt.rmf <- rc_input_rmf(dt = crops, B_DEPTH = 0.3, A_CLAY_MI = dt.soc$A_CLAY_MI,
                         dt.time = dt.time, dt.weather = dt.weather)
  
  rothc.parms <- list(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02,
                      R1 = dt.rmf$R1, abcd = dt.rmf$abcd, time = dt.rmf$time)
  
  # Amendment with B_C_OF_AMENDMENT
  amendment1 <- data.table(
    P_HC = c(0.7),
    B_C_OF_AMENDMENT = c(2215),
    P_DATE_FERTILIZATION = c("2022-05-01")
  )
  
  result1 <- rc_initialise(crops = crops, amendment = amendment1, dt.soc = dt.soc,
                           rothc.parms = rothc.parms, rothc.event = rothc.event,
                           dt.time = dt.time,
                           initialisation_method = 'spinup_analytical_heuvelink')
  
  # Amendment with P_DOSE and P_C_OF
  amendment2 <- data.table(
    P_DOSE = c(63300),
    P_HC = c(0.7),
    P_C_OF = c(35),
    P_DATE_FERTILIZATION = c("2022-05-01")
  )
  
  result2 <- rc_initialise(crops = crops, amendment = amendment2, dt.soc = dt.soc,
                           rothc.parms = rothc.parms, rothc.event = rothc.event,
                           dt.time = dt.time,
                           initialisation_method = 'spinup_analytical_heuvelink')
  
  # Both should return valid results
  expect_type(result1, "double")
  expect_type(result2, "double")
  expect_true(all(is.finite(result1)))
  expect_true(all(is.finite(result2)))
  expect_equal(result1, result2, tolerance = 1e-6)
})