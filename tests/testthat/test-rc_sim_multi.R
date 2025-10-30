# Unit tests to check if parallel rothc

testthat::source("tests/testthat/helper-data.R")

test_that("rc_sim_multi runs with normal inputs", {
  soil_properties <- create_soil_properties()[rep(1:.N, 3)][, ID := .I]
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3 
  
  rothc_rotation <- create_rotation()[rep(1:.N, 3)][, ID := rep(1:3, each = 2)]
  
  rothc_amendment <- data.table(
    ID = rep(1:3, each = 2),
    P_ID = rep(1, 6),
    P_NAME = rep('cattle_slurry', 6),
    P_DOSE = rep(c(100000, 25000, 70000), 2),
    P_HC = rep(0.7,6),
    P_C_OF = rep(35, 6),
    P_DATE_FERTILIZATION = rep(c("2022-05-01", "2023-05-01"),each=3)
  )
  
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


