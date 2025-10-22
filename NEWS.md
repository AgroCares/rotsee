# rotsee v0.1.3 2025-10-15

## added
* unit tests for `rc_multicore` and `rc_parallel`

## changed
* Reworked `rc_multicore` and `rc_parallel` for a clearer workflow for multicore calculations
* Align required inputs for `rc_multicore` with `rc_sim`
* Tightened outputs to only time, OM content, and C content of different pools

## removed
* `rc_shi_field`, with core functions incorporated into `rc_parallel`

# rotsee v0.1.2 2025-10-13
## added
* unit tests for `rc_calculate_B_C_OF` and `rc_time_period`

## changed
* Loosened checks for `rc_calculate_B_C_OF` and `rc_update_weather` to allow additional input columns

# rotsee v0.1.0 2025-09-24

## added
* B_LU_START and B_LU_END as input parameters in rothc_rotation
* start_date and end_date to replace simyears
* Helper functions rc_extend_crops and rc_extend_amendments to extend user input tables
* Helper function rc_time_period to generate a table of all years and months in the simulation period

## changed
* Base soil cover on user supplied crop growing dates
* Calculate actual evapotranspiration based on simple rothc correction factor
* Corrected accumulated soil moisture deficit calculation

# rotsee v0.0.4 2025-09-24

## Changed
* Split `rc_input_events` into `rc_input_event_crop`, `rc_input_event_amendment`, and merge these in `rc_input_events`
* Added unit tests for `rc_input_event_crop`, `rc_input_event_amendment`, and `rc_input_events`
* Expand README with some introductory text about the package
  
# rotsee v0.0.3 2025-09-01

## Added
* function rc_check_inputs to check input data
* Helper function rc_calculate_bd to estimate dry soil bulk density based on soil properties
* Helper function rc_calculate_B_C_OF to calculate crop C inputs based on crop management

## changed
* Input data of crops and amendments to be optional
* Option to input total crop C input or general crop management data
* Changed amendment inputs to single event date, format yyyy-mm-dd

# rotsee V0.0.2 2025-08-28

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
