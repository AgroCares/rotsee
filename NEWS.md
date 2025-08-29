# rotsee V0.0.2 2025-08-28
## Changed
* Split `rc_input_events` into `rc_input_event_crop`, `rc_input_event_amendment`, and merge these in `rc_input_events`
* Added unit tests for `rc_input_event_crop`, `rc_input_event_amendment`, and `rc_input_events`
* Expand README with some introductory text about the package
  
## Added
* Add GitHub Action to run R-CMD-CHECK for PR's
* Added helper functions to check input weather and parameter data, and insert default values when not supplied
* Added unit tests for helper functions

## Fixed
* Add missing dependencies for `roxygen2`, `devtools` and `usethis`

# rotsee v0.0.1 2025-07-31

## Added
* function `rc_ode` the set of ordinairy differential equations to run RothC
* function `rc_sim()` to run a simulation for RothC
* function `rc_helpers` to add supportive functions
* function `rc_input_amendment` to prepare the C input file from manure, FYM or compost
* function `rc_input_crop` to prepare the C input file from crop residues
* function `rc_input_rmf` to prepare the rate modifying factors input file
* function `rc_input_events` to prepare the event object needed for the ODE
* function `rc_input_scenario` to prepare scenarios (for NL at the moment)
* function `rc_sim` to simulate a RothC simulation for a single field
* script `rc_tables` to define structure of package tables
* function `rc_initialise` a function to select different initialisation options (draft)
* function `rc_multicore` and `rc_parallel` to allow multi-core calculations
* function `rc_shi_field` to estimate soil health index based on saturation degree (draft)
* package tables `rc_crops`, `rc_makkink` and `rc_parms`
* README, description and changelog
