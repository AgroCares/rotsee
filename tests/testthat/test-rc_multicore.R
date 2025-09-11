# Unit tests to check if multicore simultaneous RothC calculations work

test_that("rc_multicore runs with normal inputs", {
  ID <- c('high', 'low', 'normal')
  B_LU_BRP <- c(240, 314, 331)
  A_SOM_LOI <- c(10, 4, 8)
  A_CLAY_MI <- c(18, 25, 7)
  scen <- c('BAU', 'BAU', 'BAU')
  simyears <- 50
  quiet <- TRUE
  
  # Run with all correct values
  #rc_multicore(ID = ID,
  #               B_LU_BRP = B_LU_BRP,
  #               A_SOM_LOI = A_SOM_LOI,
  #              A_CLAY_MI = A_CLAY_MI,
  #               scen = scen,
  #               simyears = simyears,
  #               quiet = quiet)
})


