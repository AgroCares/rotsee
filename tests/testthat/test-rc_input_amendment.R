test_that("rc_input_amendment runs correctly using P_C_OF and P_DOSE", {
  # create amendment table (only with P_C_OF and P_DOSE)
  amendment <- create_amendment()
  
  # run script
  result <- rc_input_amendment(dt = amendment)
  
  # Check that the result is a data.table with 2 rows
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
 
  # Check that dates are correct
  expect_equal(result$month, c(5, 5))
  expect_equal(result$year, c(2022, 2023))
  
  # --- Manually calculate expected values for the amendment ---
  # P_DOSE = 63300, P_C_OF = 35, P_HC = 0.7
  fr_dpm_rpm <- -2.174 * 0.7 + 2.02
  cin_tot <- 63300 * 35/1000
  cin_hum <- 0.02 * cin_tot
  cin_dpm <- (1 - 0.02) * cin_tot * fr_dpm_rpm / (1 + fr_dpm_rpm)
  cin_rpm <- (1 - 0.02) * cin_tot - cin_dpm
  
  
  # Check calculated values for the first row (April amendment)
  expect_equal(result[1, cin_tot], cin_tot)
  expect_equal(result[1, cin_hum], cin_hum)
  expect_equal(result[1, cin_dpm], cin_dpm)
  expect_equal(result[1, cin_rpm], cin_rpm)
  
})


test_that("rc_input_amendment correctly runs with B_C_OF_AMENDMENT", {
  # create data table
  amendment <- data.table(
    P_HC = c(0.7,0.7),
    P_DATE_FERTILIZATION = c("2022-05-01", "2023-05-01"),
    B_C_OF_AMENDMENT = c(20000, 20000))
  
  # model run
  result <- rc_input_amendment(dt = amendment)
  
  # Check that the result is a data.table with 2 rows
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  
  # Check that dates are correct
  expect_equal(result$month, c(5, 5))
  expect_equal(result$year, c(2022, 2023))
  
  # --- Manually calculate expected values for the amendment ---
  fr_dpm_rpm <- -2.174 * 0.7 + 2.02
  cin_tot <- 20000
  cin_hum <- 0.02 * cin_tot
  cin_dpm <- (1 - 0.02) * cin_tot * fr_dpm_rpm / (1 + fr_dpm_rpm)
  cin_rpm <- (1 - 0.02) * cin_tot - cin_dpm
  
  
  # Check calculated values for the first row (April amendment)
  expect_equal(result[1, cin_tot], cin_tot)
  expect_equal(result[1, cin_hum], cin_hum)
  expect_equal(result[1, cin_dpm], cin_dpm)
  expect_equal(result[1, cin_rpm], cin_rpm)
  
})

test_that("rc_input_amendment correctly runs with B_C_OF_AMENDMENT and P_C_OF + P_DOSE", {
  # create data table
  amendment <- create_amendment()
  amendment[, B_C_OF_AMENDMENT := 20000]
  
  # model run
  result <- rc_input_amendment(dt = amendment)
  
  # Check that the result is a data.table with 2 rows
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  
  # Check that dates are correct
  expect_equal(result$month, c(5, 5))
  expect_equal(result$year, c(2022, 2023))
  
  # --- Manually calculate expected values for the amendment ---
  fr_dpm_rpm <- -2.174 * 0.7 + 2.02
  cin_tot <- 20000 # should be equal to B_C_OF_AMENDMENT
  cin_hum <- 0.02 * cin_tot
  cin_dpm <- (1 - 0.02) * cin_tot * fr_dpm_rpm / (1 + fr_dpm_rpm)
  cin_rpm <- (1 - 0.02) * cin_tot - cin_dpm
  
  
  # Check calculated values for the first row (April amendment)
  expect_equal(result[1, cin_tot], cin_tot)
  expect_equal(result[1, cin_hum], cin_hum)
  expect_equal(result[1, cin_dpm], cin_dpm)
  expect_equal(result[1, cin_rpm], cin_rpm)
  
})

test_that("rc_input_amendment correctly checks input validity", {
  # create amendment
  amendment <- create_amendment()
  
  # Run with no amendment provided
  expect_error(rc_input_amendment(dt = NULL), 'Must be a data.table')
  
  ## Fertilization date --------------------------
  # Run without fertilization date
  amendment_nodate <- copy(amendment)[, P_DATE_FERTILIZATION := NULL]
  expect_error(rc_input_amendment(dt = amendment_nodate), 'missing elements')
  
  # fertilization date in wrong format
  amendment_wrongdate <- copy(amendment)[, P_DATE_FERTILIZATION := 'wrong date']
  expect_error(rc_input_amendment(dt = amendment_wrongdate), 'ambiguous format')
  
  ## Humification coefficient ---------------------
  # Run without humification coefficient
  amendment_nohc <- copy(amendment)[, P_HC := NULL]
  expect_error(rc_input_amendment(dt = amendment_nohc), 'missing elements')
  
  # Humification coefficient above maximum allowed
  amendment_highhc <- copy(amendment)[, P_HC := c(1.5, 0.7)]
  expect_error(rc_input_amendment(dt = amendment_highhc), paste0('not <= ', rc_maxval('P_HC')))
  
  # negative humification coefficient
  amendment_neghc <- copy(amendment)[, P_HC := c(-0.5, 0.7)]
  expect_error(rc_input_amendment(dt = amendment_neghc), paste0('not >= ', rc_minval('P_HC')))
  
  ## B_C_OF_AMENDMENT ---------------------------------
  # Run without B_C_OF_AMENDMENT 
  amendment_nobcof <- copy(amendment)[, P_C_OF := NULL][, P_DOSE := NULL]
  expect_error(rc_input_amendment(dt = amendment_nobcof), 'Provide both P_DOSE and P_C_OF')
  
  # Run with too high B_C_OF_AMENDMENT
  amendment_highbcof <- copy(amendment_nobcof)[, B_C_OF_AMENDMENT := c(300000, 50000)]
  expect_error(rc_input_amendment(dt = amendment_highbcof), paste0('not <= ', format(rc_maxval('B_C_OF_AMENDMENT'), scientific = FALSE)))
  
  # Run with negative B_C_OF_AMENDMENT
  amendment_negbcof <- copy(amendment_nobcof)[, B_C_OF_AMENDMENT := c(-30, 50000)]
  expect_error(rc_input_amendment(dt = amendment_negbcof), paste0('not >= ', rc_minval('B_C_OF_AMENDMENT')))
  
  ## P_DOSE & P_C_OF
  # Run with only P_C_OF
  amendment_pcof <- copy(amendment)[, P_DOSE := NULL]
  expect_error(rc_input_amendment(dt = amendment_pcof), 'Provide both P_DOSE and P_C_OF')
  
  # Run with only P_DOSE
  amendment_dose <- copy(amendment)[, P_C_OF := NULL]
  expect_error(rc_input_amendment(dt = amendment_pcof), 'Provide both P_DOSE and P_C_OF')
 
  # Run with too high P_C_OF
  amendment_highpcof <- copy(amendment)[, P_C_OF := 5000]
  expect_error(rc_input_amendment(dt = amendment_highpcof), paste0('not <= ', rc_maxval('P_C_OF')))
  
  # Run with negative P_C_OF
  amendment_negpcof <- copy(amendment)[, P_C_OF := -30]
  expect_error(rc_input_amendment(dt = amendment_negpcof), paste0('not >= ', rc_minval('P_C_OF')))
  
  # Run with too high P_DOSE
  amendment_highdose <- copy(amendment)[, P_DOSE := 500000]
  expect_error(rc_input_amendment(dt = amendment_highdose), paste0('not <= ', rc_maxval('P_DOSE')))
  
  # Run with negative P_DOSE
  amendment_negdose <- copy(amendment)[, P_DOSE := -30]
  expect_error(rc_input_amendment(dt = amendment_negdose), paste0('not >= ', rc_minval('P_DOSE')))
  
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


