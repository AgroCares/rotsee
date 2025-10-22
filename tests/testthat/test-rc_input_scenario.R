# Test file for rc_input_scenario function
# Testing framework: testthat

library(testthat)
library(data.table)

test_that("rc_input_scenario validates cf_yield parameter", {
  B_LU_BRP <- c(308, 308)
  
  # Valid cf_yield values
  expect_no_error(rc_input_scenario(B_LU_BRP, scen = 'BAU', cf_yield = 0.1))
  expect_no_error(rc_input_scenario(B_LU_BRP, scen = 'BAU', cf_yield = 1.0))
  expect_no_error(rc_input_scenario(B_LU_BRP, scen = 'BAU', cf_yield = 2.0))
  
  # Invalid cf_yield - too low
  expect_error(
    rc_input_scenario(B_LU_BRP, scen = 'BAU', cf_yield = 0.05),
    "not >= 0.1"
  )
  
  # Invalid cf_yield - too high
  expect_error(
    rc_input_scenario(B_LU_BRP, scen = 'BAU', cf_yield = 2.5),
    "not <= 2"
  )
  
  # Invalid cf_yield - not numeric
  expect_error(
    rc_input_scenario(B_LU_BRP, scen = 'BAU', cf_yield = "1.0"),
    "Must be of type 'numeric'"
  )
  
  # Invalid cf_yield - multiple values
  expect_error(
    rc_input_scenario(B_LU_BRP, scen = 'BAU', cf_yield = c(1.0, 1.2)),
    "Must have length 1"
  )
  
  # Invalid cf_yield - NA
  expect_error(
    rc_input_scenario(B_LU_BRP, scen = 'BAU', cf_yield = NA_real_),
    "Contains missing values"
  )
})

test_that("rc_input_scenario validates B_LU_BRP parameter", {
  # Valid B_LU_BRP
  expect_no_error(rc_input_scenario(c(308, 308), scen = 'BAU'))
  expect_no_error(rc_input_scenario(c(233), scen = 'BAU'))
  
  # Invalid B_LU_BRP - not in crop codes
  expect_error(
    rc_input_scenario(c(99999), scen = 'BAU'),
    "has additional elements"
  )
  
  # Invalid B_LU_BRP - not integer-ish
  expect_error(
    rc_input_scenario(c(308.5), scen = 'BAU'),
    "integerish"
  )
  
  # Invalid B_LU_BRP - contains NA
  expect_error(
    rc_input_scenario(c(308, NA), scen = 'BAU'),
    "Contains missing values"
  )
})

test_that("rc_input_scenario returns correct structure for BAU scenario", {
  B_LU_BRP <- c(308, 233, 308)
  result <- rc_input_scenario(B_LU_BRP, scen = 'BAU')
  
  # Check output structure
  expect_type(result, "list")
  expect_named(result, c("rotation", "amendment"))
  expect_s3_class(result$rotation, "data.table")
  expect_s3_class(result$amendment, "data.table")
  
  # Check rotation columns
  expect_true(all(c("year", "B_LU_EOM", "B_LU_EOM_RESIDUE", "B_LU_HC",
                    "M_GREEN_TIMING", "M_CROPRESIDUE", "M_IRRIGATION",
                    "cf_yield", "B_LU") %in% names(result$rotation)))
  
  # Check amendment columns
  expect_true(all(c("year", "P_NAME", "month", "P_OM", "P_HC", 
                    "p_p2o5", "P_DOSE") %in% names(result$amendment)))
  
  # Check M_IRRIGATION is FALSE by default
  expect_true(all(result$rotation$M_IRRIGATION == FALSE))
  
  # Check number of rows matches input
  expect_equal(nrow(result$rotation), length(B_LU_BRP))
})

test_that("rc_input_scenario includes cf_yield in rotation output", {
  B_LU_BRP <- c(308, 308)
  cf_yield_val <- 1.5
  result <- rc_input_scenario(B_LU_BRP, scen = 'BAU', cf_yield = cf_yield_val)
  
  # Check cf_yield is in rotation
  expect_true("cf_yield" %in% names(result$rotation))
  expect_equal(unique(result$rotation$cf_yield), cf_yield_val)
})

test_that("rc_input_scenario handles different scenarios correctly", {
  B_LU_BRP <- c(308, 233, 308)
  
  scenarios <- c('BAU', 'BAUIMPR', 'CLT', 'ALL')
  
  for (scen in scenarios) {
    result <- rc_input_scenario(B_LU_BRP, scen = scen)
    
    # Check structure is valid for all scenarios
    expect_type(result, "list", info = paste("Scenario:", scen))
    expect_named(result, c("rotation", "amendment"), info = paste("Scenario:", scen))
    expect_s3_class(result$rotation, "data.table", info = paste("Scenario:", scen))
    expect_s3_class(result$amendment, "data.table", info = paste("Scenario:", scen))
    
    # Check M_IRRIGATION exists
    expect_true("M_IRRIGATION" %in% names(result$rotation), 
                info = paste("Scenario:", scen))
  }
})

test_that("rc_input_scenario BAUIMPR scenario includes compost", {
  B_LU_BRP <- c(308, 308)
  result <- rc_input_scenario(B_LU_BRP, scen = 'BAUIMPR')
  
  # BAUIMPR should include both cattle_slurry and green_compost
  expect_true(any(grepl("cattle_slurry", result$amendment$P_NAME)))
  expect_true(any(grepl("green_compost", result$amendment$P_NAME)))
  
  # Should have more amendments than BAU
  result_bau <- rc_input_scenario(B_LU_BRP, scen = 'BAU')
  expect_gt(nrow(result$amendment), nrow(result_bau$amendment))
})

test_that("rc_input_scenario CLT scenario enables crop residue", {
  B_LU_BRP <- c(308, 308)
  result <- rc_input_scenario(B_LU_BRP, scen = 'CLT')
  
  # CLT should have M_CROPRESIDUE = TRUE
  expect_true(all(result$rotation$M_CROPRESIDUE == TRUE))
  
  # Compare to BAU which should have FALSE for these crops
  result_bau <- rc_input_scenario(B_LU_BRP, scen = 'BAU')
  expect_true(all(result_bau$rotation$M_CROPRESIDUE == FALSE))
})

test_that("rc_input_scenario handles single crop correctly", {
  B_LU_BRP <- c(308)
  result <- rc_input_scenario(B_LU_BRP, scen = 'BAU')
  
  expect_equal(nrow(result$rotation), 1)
  expect_true(nrow(result$amendment) >= 0)
  expect_true("M_IRRIGATION" %in% names(result$rotation))
})

test_that("rc_input_scenario handles long rotation correctly", {
  B_LU_BRP <- rep(c(308, 233, 265), 10)  # 30 years
  result <- rc_input_scenario(B_LU_BRP, scen = 'BAU')
  
  expect_equal(nrow(result$rotation), length(B_LU_BRP))
  expect_true(all(result$rotation$M_IRRIGATION == FALSE))
  expect_true("cf_yield" %in% names(result$rotation))
})

test_that("rc_input_scenario default cf_yield is 1", {
  B_LU_BRP <- c(308, 308)
  result <- rc_input_scenario(B_LU_BRP, scen = 'BAU')
  
  # Default cf_yield should be 1
  expect_equal(unique(result$rotation$cf_yield), 1)
})

test_that("rc_input_scenario M_IRRIGATION column is always FALSE", {
  B_LU_BRP <- c(308, 233, 265, 259)
  
  scenarios <- c('BAU', 'BAUIMPR', 'CLT', 'ALL')
  
  for (scen in scenarios) {
    result <- rc_input_scenario(B_LU_BRP, scen = scen)
    
    # M_IRRIGATION should always be FALSE (default)
    expect_true(all(result$rotation$M_IRRIGATION == FALSE), 
                info = paste("Scenario:", scen))
  }
})