#' Helper function to weight and correct the risk and scores
#'
#' @param x The risk or score value to be weighted
#'
#' @examples
#' cf_ind_importance(x = 0.5)
#' cf_ind_importance(x = c(0.1,0.5,1.5))
#'
#' @return
#' A transformed variable after applying a inverse weighing function so that lower values will gain more impact when applied in a weighed.mean function. A numeric value.
#'
#' @export
cf_ind_importance <- function(x) {
  y <- 1 / (x  + 0.2)
  
  return(y)
}




#' Function to check weather table and derive default table when not given as input
#'
#' @param dt (Data table) Table with following column names: month, W_TEMP_MEAN_MONTH, W_PREC_MEAN_MONTH, W_ET_POT_MONTH, W_ET_ACT_MONTH.
#' 
#' @returns
#' A data table containing monthly data of temperature, precipitation, and evapotranspiration.
#' 
#' @export
#'
rc_update_weather <- function(dt){
  if(is.null(dt)){
    # Set default weather for Dutch conditions
    dt <- data.table(month = 1:12,
                             W_TEMP_MEAN_MONTH = c(3.6,3.9,6.5,9.8,13.4,16.2,18.3,17.9,14.7,10.9,7,4.2),
                             W_PREC_MEAN_MONTH = c(70.8, 63.1, 57.8, 41.6, 59.3, 70.5, 85.2, 83.6, 77.9, 81.1, 80.0, 83.8),
                             W_ET_POT_MONTH = c(8.5, 15.5, 35.3, 62.4, 87.3, 93.3, 98.3, 82.7, 51.7, 28.0, 11.3,  6.5),
                             W_ET_ACT_MONTH = NA_real_)
}else{
  # Create weather data table
  dt <- copy(dt)
}
  # Check inputs
  checkmate::assert_data_table(dt)
  checkmate::assert_subset(c("month", "W_TEMP_MEAN_MONTH", "W_PREC_MEAN_MONTH"), colnames(dt))
  checkmate::assert(
    any(c("W_ET_POT_MONTH", "W_ET_ACT_MONTH") %in% names(dt)),
    msg = "At least one of 'W_ET_POT_MONTH' or 'W_ET_ACT_MONTH' must be provided."
  )
  checkmate::assert_subset(dt$month, 1:12, add = FALSE)
  checkmate::assert_numeric(dt$W_TEMP_MEAN_MONTH, lower = -30, upper = 50, any.missing = FALSE, len = 12)
  checkmate::assert_numeric(dt$W_PREC_MEAN_MONTH, lower = 0, upper = 10000, any.missing = FALSE, len = 12)

  # Check if both potential and actual ET are provided
  if ("W_ET_POT_MONTH" %in% colnames(dt) && "W_ET_act_MONTH" %in% colnames(dt)) {
        checkmate::assert(
      !all(is.na(dt$W_ET_POT_MONTH)) || !all(is.na(dt$W_ET_act_MONTH)),
      msg = "At least on of W_ET_POT_MONTH and W_ET_act_MONTH should not contain NA values."
    )
    # Check ranges, allow NAs
    checkmate::assertNumeric(dt$W_ET_POT_MONTH, lower = 0, upper = 1000, any.missing = TRUE)
    checkmate::assertNumeric(dt$W_ET_act_MONTH, lower = 0, upper = 10000, any.missing = TRUE)
  } else if ("W_ET_POT_MONTH" %in% colnames(dt)) {
    # Only potential ET provided: no NA allowed
    checkmate::assertNumeric(dt$W_ET_POT_MONTH, lower = 0, upper = 1000, any.missing = FALSE)
  } else if ("W_ET_act_MONTH" %in% colnames(dt)) {
    # Only actual ET provided: no NA allowed
    checkmate::assertNumeric(dt$W_ET_act_MONTH, lower = 0, upper = 10000, any.missing = FALSE)
  }
  return(dt)
}


