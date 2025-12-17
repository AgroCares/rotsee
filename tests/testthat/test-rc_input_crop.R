source("helper-testdata.R")

test_that("rc_input_crop runs correctly", {
  # create crop table
  crop <- create_rotation()
  
  # run script
  result <- rc_input_crop(dt = crop)
  
  # Check that the result is a data.table with 2 rows
  expect_s3_class(result, "data.table")
  expect_equal(nrow(result), 2)
  
  # Check that dates are correct
  expect_equal(result$month, c(10, 10))
  expect_equal(result$year, c(2022, 2023))
  
  # --- Manually calculate expected values for the crop ---
  # B_C_OF_CULT = 1500, P_HC = 0.32
  fr_dpm_rpm <- -2.174 * 0.32 + 2.02
  cin_tot <- 1500
  cin_dpm <- cin_tot * fr_dpm_rpm / (1 + fr_dpm_rpm)
  cin_rpm <- cin_tot - cin_dpm
  
  
  # Check calculated values for the first row (April amendment)
  expect_equal(result[1, cin_dpm], cin_dpm)
  expect_equal(result[1, cin_rpm], cin_rpm)
  expect_equal(result[1, cin_dpm] + result[1, cin_rpm], cin_tot)
})

test_that("rc_input_crop validates inputs correctly", {
  # Create crop table
  crop <- create_rotation()
  
  # Run with no crop input provided
  expect_error(rc_input_crop(dt = NULL), 'Must be a data.table')
  
  ## B_C_OF_CULT ------------------
  # Run without B_C_OF_CULT
  crop_nobcof <- copy(crop)[, B_C_OF_CULT := NULL]
  expect_error(rc_input_crop(dt = crop_nobcof), 'missing elements')
  
  # Too high B_C_OF_CULT
  crop_highbcof <- copy(crop)[, B_C_OF_CULT := 500000]
  expect_error(rc_input_crop(dt = crop_highbcof), paste0('not <= ', format(rc_maxval('B_C_OF_CULT'), scientific = FALSE)))
  
  # negative B_C_OF_CULT
  crop_negbcof <- copy(crop)[, B_C_OF_CULT := -30]
  expect_error(rc_input_crop(dt = crop_negbcof), paste0('not >= ', rc_minval('B_C_OF_CULT')))
  
  ## B_LU_START --------------------------
  # Run without B_LU_START
  crop_nostart <- copy(crop)[, B_LU_START := NULL]
  expect_error(rc_input_crop(dt = crop_nostart), 'missing elements')
  
  # B_LU_START in wrong format
  crop_wrongstart <- copy(crop)[, B_LU_START := 'wrong date']
  expect_error(rc_input_crop(dt = crop_wrongstart), 'ambiguous format')
  
  ## B_LU_END --------------------------
  # Run without B_LU_END
  crop_noend <- copy(crop)[, B_LU_END := NULL]
  expect_error(rc_input_crop(dt = crop_noend), 'missing elements')
  
  # B_LU_END in wrong format
  crop_wrongend <- copy(crop)[, B_LU_END := 'wrong date']
  expect_error(rc_input_crop(dt = crop_wrongend), 'ambiguous format')
  
  # Humification coefficient ---------------------
  # Run without humification coefficient
  crop_nohc <- copy(crop)[, B_LU_HC := NULL]
  expect_error(rc_input_crop(dt = crop_nohc), 'missing elements')
  
  # Humification coefficient above maximum allowed
  crop_highhc <- copy(crop)[, B_LU_HC := 1.5]
  expect_error(rc_input_crop(dt = crop_highhc), paste0('not <= ', rc_maxval('B_LU_HC')))
  
  # negative humification coefficient
  crop_neghc <- copy(crop)[, B_LU_HC := -0.5]
  expect_error(rc_input_crop(dt = crop_neghc), paste0('not >= ', rc_minval('B_LU_HC')))
})