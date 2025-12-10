#' RothC ordinary differential equation
#'
#' Set of differential equations to calculate the evolution of Soil Organic Carbon stocks using the RothC model.
#'
#' @param y state variables to run RothC
#' @param time (numeric) time to print output
#' @param parms (list) parameters to run RothC, includes rate modifying functions and constants R1 and abc
#'
#' @import deSolve
#'
#' @export
rc_ode <- function (time, y, parms) {
  
  with(as.list(c(y,parms)),{
    dCDPM   <- abcd(time)*-k1*CDPM
    dCRPM   <- abcd(time)*-k2*CRPM
    dCBIO   <- abcd(time)*-k3*CBIO - (abcd(time)*-k3*CBIO*R1*0.46)-(dCDPM*R1*0.46)-(dCRPM*R1*0.46)-(abcd(time)*-k4*CHUM*R1*0.46)
    dCHUM   <- abcd(time)*-k4*CHUM - (abcd(time)*-k4*CHUM*R1*0.54)-(dCDPM*R1*0.54)-(dCRPM*R1*0.54)-(abcd(time)*-k3*CBIO*R1*0.54)
    list(c(dCDPM,dCRPM,dCBIO,dCHUM))})
}