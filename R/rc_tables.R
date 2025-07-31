#' Crop proporties table
#'
#' This table has carbon related input parameters for crops
#'
#' @format A data.frame with 521 rows and 8 columns:
#' \describe{
#'   \item{crop_code}{The BRP gewascode of the crop}
#'   \item{crop_name}{The name of the crop, in lower case}
#'   \item{crop_cat1}{Classification of crop per land use type (arable, maize, grass, nature)}
#'   \item{country}{The country name where the crop codes are applicable}
#'   \item{B_LU}{The international crop code used in pandex}
#'   \item{B_LU_EOM}{The effective organic matter supply via roots and root exudates (kg EOS/ha)}
#'   \item{B_LU_EOM_RESIDUE}{The effective organic matter supply via crop residues (kg EOS/ha)}
#'   \item{B_LU_HC}{The humification coefficient for the crop remainings left in soil after harvest}
#'   \item{B_LU_WATERSTRESS_OBIC}{A crop category used in OBIC to express sensitivity to water stress}
#'   \item{B_LU_MAKKINK}{A crop category used to link crop to Makkink correction factor to estimate actual evaporation}
#' }
"rc_crops"

#' Table with input parameters being used in the package
#'
#' This table contains all parameters being used in the rotsee package.
#'
#' @format A data.table with x rows and x columns:
#' \describe{
#'   \item{rc_parm_id}{the parameter id}
#'   \item{rc_parm_name}{the name of the parameter}
#'   \item{rc_parm_type}{the type of the parameter. Options: measurement, field property}
#'   \item{rc_parm_description}{a short description of the parameters}
#'   \item{rc_parm_unit}{the unit of the parameter}
#'   \item{rc_parm_min}{the maximum allowed value for the parameter}
#'   \item{rc_parm_max}{the minimum allowed value for the parameter}
#'   \item{rc_parm_data_type}{the data type of the parameter: numeric, character or boolean}
#'   \item{rc_parm_enum}{does the parameter have predefined options}
#'   \item{rc_parm_options}{allowed options for the parameteer}
#'}
"rc_parms"