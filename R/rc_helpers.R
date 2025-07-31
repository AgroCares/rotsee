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
