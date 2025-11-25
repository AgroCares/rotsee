

test_that("rc_sim_multistep handles correct inputs", {
  this.xs <- 1
  
  soil_properties <- data.table(
    ID = c('high', 'low', 'mid'),
    xs = c(1,2,3),
    A_C_OF = rep(80,3),
    B_C_ST03 = rep(2100,3),
    A_CLAY_MI = rep(18,3),
    A_DENSITY_SA = rep(1.4,3)
  )
  
  rothc_rotation <- data.table(
    ID = rep(c('high', 'low', 'mid'),2),
    xs = rep(c(1,2,3),2),
    B_LU_START = rep(c("2022-04-01", "2023-04-01"),each=3),
    B_LU_END = rep(c("2022-10-01", "2023-10-01"),each=3),
    B_LU_HC = rep(0.32, 6),
    B_C_OF_INPUT = rep(1500, 6)
  )
  
  rothc_amendment <- data.table(
    ID = rep(c('high', 'low', 'mid'),2),
    xs = rep(c(1,2,3),2),
    P_ID = rep(1, 6),
    P_NAME = rep('cattle_slurry', 6),
    P_DOSE = rep(c(100000, 25000, 70000), 2),
    P_HC = rep(0.7,6),
    P_C_OF = rep(35, 6),
    P_DATE_FERTILIZATION = rep(c("2022-05-01", "2023-05-01"),each=3)
  )
  
  weather <- data.table(month = 1:12,
                        W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                        W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                        W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                        W_ET_ACT_MONTH = NA_real_)
  
  parms <- list(dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
                c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
                initialize = TRUE,
                unit = "A_SOM_LOI",
                method = "adams",
                poutput = "year",
                start_date = "2022-04-01",
                end_date = "2040-10-01")
  
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

