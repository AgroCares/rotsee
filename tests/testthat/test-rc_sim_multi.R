# Unit tests to check if parallel rothc

source("helper-testdata.R")

test_that("rc_sim_multi runs with normal inputs", {
  soil_properties <- create_soil_properties()[rep(1:.N, 3)][, ID := .I]
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3 
  
  rothc_rotation <- create_rotation()[rep(1:.N, 3)][, ID := rep(1:3, each = 2)]
  
  rothc_amendment <- create_amendment()[rep(1:.N, 3)][, ID := rep(1:3, each = 2)]
  rothc_amendment[, P_DOSE := rep(c(100000, 25000, 70000), 2)]
  
  weather <- create_weather()
  
  parms <- create_parms()
  
  
  # Run with all correct values
  result <- rc_sim_multi(soil_properties = soil_properties,
                         A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH,
                         parms = parms,
                         weather = weather,
                         rotation = rothc_rotation,
                         amendment = rothc_amendment,
                         final = FALSE,
                         strategy = 'multisession')

  expect_s3_class(result, "data.table")
  expect_true(all(c("ID", "A_SOM_LOI", "soc", "xs") %in% names(result)))
  expect_equal(nrow(result), 57)


  # Runs correctly with final set to true
  result_final <- rc_sim_multi(soil_properties = soil_properties,
                         A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH,
                         parms = parms,
                         weather = weather,
                         rotation = rothc_rotation,
                         amendment = rothc_amendment,
                         final = TRUE,
                         strategy = 'multisession')

  
  expect_equal(nrow(result_final), 3)
})


test_that("rc_sim_multi throws error if soil_properties is not a data.table", {
  expect_error(rc_sim_multi(soil_properties = data.frame(ID = "test")))
})


test_that("rc_sim_multi handles progress reporting", {
  soil_properties <- create_soil_properties()[rep(1:.N, 3)][, ID := .I]
  
  
  rothc_rotation <- create_rotation()[rep(1:.N, 3)][, ID := rep(1:3, each = 2)]
  
  
  rothc_amendment <- create_amendment()[rep(1:.N, 3)][, ID := rep(1:3, each = 2)]
  rothc_amendment[, P_DOSE := rep(c(100000, 25000, 70000), 2)]
  
  weather <- create_weather()
  
  parms <- create_parms()
  
  # Test with progress reporting
  result <- rc_sim_multi(soil_properties = soil_properties,
                         A_DEPTH = 0.3,
                         B_DEPTH = 0.3,
                         parms = parms,
                         weather = weather,
                         rotation = rothc_rotation,
                         amendment = rothc_amendment,
                         quiet = FALSE,
                         strategy = 'multisession')
  
  expect_s3_class(result, "data.table")
})

test_that("rc_sim_multi runs when cores are defined", {
  # read in input data
  soil_properties <- create_soil_properties()[rep(1:.N, 3)][, ID := .I]
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3 
  
  rothc_rotation <- create_rotation()[rep(1:.N, 3)][, ID := rep(1:3, each = 2)]
  
  rothc_amendment <- create_amendment()[rep(1:.N, 3)][, ID := rep(1:3, each = 2)]
  rothc_amendment[, P_DOSE := rep(c(100000, 25000, 70000), 2)]
  
  weather <- create_weather()
  
  parms <- create_parms()
  
  # Run with cores defined
  result <- rc_sim_multi(soil_properties = soil_properties,
                         A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH,
                         parms = parms,
                         weather = weather,
                         rotation = rothc_rotation,
                         amendment = rothc_amendment,
                         final = FALSE,
                         strategy = 'multisession',
                         cores = 2)
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("ID", "A_SOM_LOI", "soc", "xs") %in% names(result)))
  expect_equal(nrow(result), 57)
})


test_that("rc_sim_multi correctly runs different future plans", {
  # read in input data
  soil_properties <- create_soil_properties()[rep(1:.N, 3)][, ID := .I]
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3 
  
  rothc_rotation <- create_rotation()[rep(1:.N, 3)][, ID := rep(1:3, each = 2)]
  
  rothc_amendment <- create_amendment()[rep(1:.N, 3)][, ID := rep(1:3, each = 2)]
  rothc_amendment[, P_DOSE := rep(c(100000, 25000, 70000), 2)]
  
  weather <- create_weather()
  
  parms <- create_parms()
  
  # multicore calculation
  result <- rc_sim_multi(soil_properties = soil_properties,
                         A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH,
                         parms = parms,
                         weather = weather,
                         rotation = rothc_rotation,
                         amendment = rothc_amendment,
                         final = FALSE,
                         strategy = 'multicore')
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("ID", "A_SOM_LOI", "soc", "xs") %in% names(result)))
  expect_equal(nrow(result), 57)
  
  # sequential calculation
  result <- rc_sim_multi(soil_properties = soil_properties,
                         A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH,
                         parms = parms,
                         weather = weather,
                         rotation = rothc_rotation,
                         amendment = rothc_amendment,
                         final = FALSE,
                         strategy = 'sequential')
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("ID", "A_SOM_LOI", "soc", "xs") %in% names(result)))
  expect_equal(nrow(result), 57)
  
})