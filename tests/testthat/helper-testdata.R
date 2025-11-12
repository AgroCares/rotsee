# This file contains helper functions to generate data for the unit tests of the rotsee package


# Helper function to generate a weather table for unit testing
create_weather <- function(){
  dt <- data.table(month = 1:12,
                   W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                   W_PREC_SUM_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                   W_ET_REF_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                   W_ET_ACT_MONTH = NA_real_)
  
  return(dt)
}

# Helper function to generate a data table with rotation information for unit testing

create_rotation <- function(){
  dt <- data.table(
    B_LU_START = c("2022-04-01", "2023-04-01"),
    B_LU_END = c("2022-10-01", "2023-10-01"),
    B_LU = c("nl_308", "nl_308"),
    B_LU_NAME = c("erwten (droog te oogsten)", "erwten (droog te oogsten)" ),
    B_LU_HC = c(0.32, 0.32),
    B_C_OF_CULT = c(1500, 1500)
  )
  
  return(dt)
}

# Helper function to generate a soil properties data table for unit testing

create_soil_properties <- function(){
  dt <- data.table(
    A_C_OF = 50,
    B_C_ST03 = 210,
    A_CLAY_MI = 18,
    A_DENSITY_SA = 1.4
  )
  return(dt)
}


# Helper function to generate an amendment data table for unit testing

create_amendment <- function(){
  dt <- data.table(
    P_ID = c(1, 1),
    P_NAME = c('cattle_slurry', 'cattle_slurry'),
    P_DOSE = c(63300, 63300),
    P_HC = c(0.7,0.7),
    P_C_OF = c(35, 35),
    P_DATE_FERTILIZATION = c("2022-05-01", "2023-05-01"))
  
  return(dt)
}



# Helper function to generate a parameter data table for unit testing

create_parms <- function(){
  dt <- list(dec_rates = c(k1 = 10, k2 = 0.3, k3 = 0.66, k4 = 0.02),
             c_fractions = c(fr_IOM = 0.049, fr_DPM = 0.015, fr_RPM = 0.125, fr_BIO = 0.015),
             initialize = TRUE,
             unit = "A_SOM_LOI",
             method = "adams",
             poutput = "year",
             start_date = "2022-04-01",
             end_date = "2040-10-01")
  
  return(dt)
}

# Helper function to generate crop events table
create_event_rotation <- function(){
  dt <- data.table(
    B_LU = c("nl_308", "nl_252"),
    year = c(2020, 2021),
    month = c(8, 8),
    cin_dpm = c(500, 750),
    cin_rpm = c(300, 450)
  )
  
  return(dt)
}


# Helper function to generate amendment events table
create_event_amendment <- function(){
  
  dt <- data.table(
    P_ID = c(1, 2, 3),
    P_NAME = c("low", "mid", "high"),
    year = c(2020, 2021, 2022),
    month = c(4, 5, 6),
    cin_tot = c(1000, 1500, 2000),
    cin_hum = c(100, 150, 200),
    cin_dpm = c(300, 450, 600),
    cin_rpm = c(600, 900, 1200)
  )
  
  return(dt)
  }

# Helper function to generate irrigation table
create_irrigation <- function(){
  
  dt <-data.table(
    B_DATE_IRRIGATION = c("2022-07-01", "2023-07-01", "2024-06-15"),
    B_IRR_AMOUNT = c(20, 25, 18)
  )
  
  return(dt)
}