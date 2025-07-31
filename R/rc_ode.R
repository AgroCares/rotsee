#' RothC ordinary differential equation
#'
#' Set of differential equations to calculate the evolution of Soil Organic Carbon stocks using the RothC model.
#'
#' @param y state variables to run RothC
#' @param time (numeric) time to print output
#' @param parms (list) parameters to run RothC, includes rate modifying functions and constants R1, abc, and d
#'
#' @import deSolve
#'
#' @export
rc_ode <- function (time, y, parms) {
  
  with(as.list(c(y,parms)),{
    dCDPM   <- abc(time)*-k1*CDPM
    dCRPM   <- abc(time)*-k2*CRPM + (d(time) * CHUM * 0.33)
    dCBIO   <- abc(time)*-k3*CBIO - (abc(time)*-k3*CBIO*R1*0.46)-(dCDPM*R1*0.46)-(dCRPM*R1*0.46)-(abc(time)*-k4*CHUM*R1*0.46)
    dCHUM   <- abc(time)*-k4*CHUM - (abc(time)*-k4*CHUM*R1*0.54)-(dCDPM*R1*0.54)-(dCRPM*R1*0.54)-(abc(time)*-k3*CBIO*R1*0.54) - (d(time) * CHUM * 0.33)
    list(c(dCDPM,dCRPM,dCBIO,dCHUM))})
}