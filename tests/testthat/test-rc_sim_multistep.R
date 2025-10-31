testthat::source_file("helper-testdata.R")

test_that("rc_sim_multistep handles correct inputs", {
  this.xs <- 1
  
  soil_properties <- create_soil_properties()[rep(1:.N, 3)][, ID := .I][, xs := .I]
  
  rothc_rotation <- create_rotation()[rep(1:.N, 3)][, ID := rep(1:3, each = 2)][, xs := rep(1:3, each = 2)]
  
  rothc_amendment <- create_amendment()[rep(1:.N, 3)][, ID := rep(1:3, each = 2)][, xs := rep(1:3, each = 2)]
  rothc_amendment[, P_DOSE := rep(c(100000, 25000, 70000), 2)]
    
  weather <- create_weather()
  
  parms <- create_parms()
  
  # Test with all correct values, final = TRUE
  result <- rc_sim_multistep(this.xs = this.xs,
                        soil_properties = soil_properties,
                        A_DEPTH = 0.3,
                        B_DEPTH = 0.3,
                        parms = parms,
                        weather = weather,
                        rotation = rothc_rotation,
                        amendment = rothc_amendment,
                        p = NULL,
                        final = TRUE)
  
  expect_s3_class(result, "data.table")
  expect_true(all(c("year", "A_SOM_LOI", "soc", "xs") %in% names(result)))
  expect_equal(nrow(result), 1)
  
  
  # test if rc_sim_multistep handles final = FALSE can be run
  result_finalf <- rc_sim_multistep(this.xs = this.xs,
                        soil_properties = soil_properties,
                        A_DEPTH = 0.3,
                        B_DEPTH = 0.3,
                        parms = parms,
                        weather = weather,
                        rotation = rothc_rotation,
                        amendment = rothc_amendment,
                        p = NULL,
                        final = FALSE)
  
  expect_s3_class(result_finalf, "data.table")
  expect_true(all(c("A_SOM_LOI", "soc", "xs") %in% names(result_finalf)))
  expect_gt(nrow(result_finalf), 1)
  
  
  # Test if rc_sim_multistep handles xs not present in soil_properties
  
  result_noxs <- rc_sim_multistep(this.xs = 99,
                        soil_properties = soil_properties,
                        A_DEPTH = 0.3,
                        B_DEPTH = 0.3,
                        parms = parms,
                        weather = weather,
                        rotation = rothc_rotation,
                        amendment = rothc_amendment,
                        p = NULL,
                        final = TRUE)
  
  expect_s3_class(result_noxs, "data.table")
  expect_true("error" %in% names(result_noxs))
  expect_true(grepl("missing", result_noxs$error))
  
  # Check that error is supplied given erroneous parms
  broken_parms <- parms
  broken_parms$dec_rates <- "not a numeric vector"
  
  result <- rc_sim_multistep(this.xs = this.xs,
                        soil_properties = soil_properties,
                        A_DEPTH = 0.3,
                        B_DEPTH = 0.3,
                        parms = broken_parms,
                        weather = weather,
                        rotation = rothc_rotation,
                        amendment = rothc_amendment,
                        p = NULL,
                        final = TRUE)
  
  expect_s3_class(result, "data.table")
  expect_true("error" %in% names(result))
  expect_true(!is.na(result$error))

})

