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

crops <- data.table(
  B_LU_START = c("2022-04-01", "2023-04-01"),
  B_LU_END = c("2022-10-01", "2023-10-01"),
  B_LU = c("nl_308", "nl_308"),
  B_LU_NAME = c("erwten (droog te oogsten)", "erwten (droog te oogsten)" ),
  B_LU_HC = c(0.32, 0.32),
  B_C_OF_INPUT = c(1500, 1500)
)

amendment <- data.table(
  P_ID = c(1, 1),
  P_NAME = c('cattle_slurry', 'cattle_slurry'),
  P_DOSE = c(63300, 63300),
  P_HC = c(0.7,0.7),
  P_C_OF = c(35, 35),
  P_DATE_FERTILIZATION = c("2022-05-01", "2023-05-01"))

soil_properties <- list(
  A_C_OF = 50,
  B_C_ST03 = 210,
  A_CLAY_MI = 18,
  A_DENSITY_SA = 1.4
)

# Calculate rothc.parms
dt.weather <- data.table(month = 1:12,
                         W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                         W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                         W_ET_POT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                         W_ET_ACT_MONTH = NA_real_)

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
                    abc = dt.rmf$abc,
                    time = dt.rmf$time)


 


# Check with all valid inputs
# spinup_analytical_bodemcoolstof
expect_no_error(rc_initialise(crops = crops,
                              amendment = amendment,
                              dt.soc = dt.soc,
                              rothc.parms = rothc.parms,
                              rothc.event = rothc.event,
                              start_date = start_date,
                              dt.time = dt.time,
                              soil_properties = soil_properties,
                              dt.weather = dt.weather,
                              initialization_method = 'spinup_analytical_bodemcoolstof'))

# spinup_analytical_heuvelink
expect_no_error(rc_initialise(crops = crops,
                              amendment = amendment,
                              dt.soc = dt.soc,
                              rothc.parms = rothc.parms,
                              rothc.event = rothc.event,
                              start_date = start_date,
                              dt.time = dt.time,
                              soil_properties = soil_properties,
                              dt.weather = dt.weather,
                              initialization_method = 'spinup_analytical_heuvelink'))

# spinup_simulation
expect_no_error(rc_initialise(crops = crops,
                              amendment = amendment,
                              dt.soc = dt.soc,
                              rothc.parms = rothc.parms,
                              rothc.event = rothc.event,
                              start_date = start_date,
                              dt.time = dt.time,
                              soil_properties = soil_properties,
                              dt.weather = dt.weather,
                              initialization_method = 'spinup_simulation'))
})