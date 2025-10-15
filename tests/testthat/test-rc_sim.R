# Test file for rc_sim 
# Testing framework: testthat

test_that("rc_sim correctly checks input validity", {
  soil_properties <- list(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  
  A_DEPTH = 0.3
  
  B_DEPTH = 0.3

  
  M_TILLAGE_SYSTEM = 'CT'
  
  rothc_rotation <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)", "erwten (droog te oogsten)" ),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_INPUT = c(1500, 1500)
    )
 
  rothc_amendment <- data.table(
    P_ID = c(1, 1),
    P_NAME = c('cattle_slurry', 'cattle_slurry'),
    P_DOSE = c(63300, 63300),
    P_HC = c(0.7,0.7),
    P_C_OF = c(35, 35),
    P_DATE_FERTILIZATION = c("2022-05-01", "2023-05-01"))
  

      
  weather <- data.table(month = 1:12,
                        W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                        W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                        W_ET_POT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                        W_ET_ACT_MONTH = NA_real_,
                        W_POT_TO_ACT = rep(0.75, 12))
  
  parms <- list(dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
                      c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
                      initialize = TRUE,
                      unit = "A_SOM_LOI",
                      method = "adams",
                      poutput = "year",
                      start_date = "2022-04-01",
                      end_date = "2040-10-01")
  
  # Set irrigation moments
  irrigation <- data.table(
    B_DATE_IRRIGATION = c("2022-07-01", "2024-07-01", "2028-06-01"),
    B_IRR_AMOUNT = c(12, 20, 10)
  )

  # All correct
 expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                   rothc_rotation = rothc_rotation, rothc_amendment = rothc_amendment, 
                   weather = weather, rothc_parms = parms, irrigation = irrigation))
  
  # No amendment table (allowed)
  expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                         B_DEPTH = B_DEPTH, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                         rothc_rotation = rothc_rotation, rothc_amendment = NULL, 
                         weather = weather, rothc_parms = parms, irrigation = irrigation))
  
    # No crop table (not allowed)
  expect_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
                   B_DEPTH = B_DEPTH,  M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
                   rothc_rotation = NULL, rothc_amendment = rothc_amendment, 
                   weather = weather, irrigation = irrigation))
  
  # Simulation longer than rotation and amendment tables
  #parms1 <- parms[, end_date := "2030-10-01"]
  #expect_no_error(rc_sim(soil_properties = soil_properties, A_DEPTH = A_DEPTH,
  #                       B_DEPTH = B_DEPTH, cf_yield = cf_yield, M_TILLAGE_SYSTEM = M_TILLAGE_SYSTEM,
  #                       rothc_rotation = rothc_rotation, rothc_amendment = NULL, 
  #                       weather = weather))
})

