#' Crop properties table
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


#' Makkink correction factor table
#'
#' This table contains the makkink correction factors for evapo-transpiration per month
#'
#' @format A data.table with 12 rows and 4 columns:
#' \describe{
#'   \item{crop_makkink}{Makkink crop category}
#'   \item{1}{Evapotranspiration correction factors for January}
#'   \item{2}{Evapotranspiration correction factors for February}
#'   \item{3}{Evapotranspiration correction factors for March}
#'   \item{4}{Evapotranspiration correction factors for April}
#'   \item{5}{Evapotranspiration correction factors for May}
#'   \item{6}{Evapotranspiration correction factors for June}
#'   \item{7}{Evapotranspiration correction factors for July}
#'   \item{8}{Evapotranspiration correction factors for August}
#'   \item{9}{Evapotranspiration correction factors for September}
#'   \item{10}{Evapotranspiration correction factors for October}
#'   \item{11}{Evapotranspiration correction factors for November}
#'   \item{12}{Evapotranspiration correction factors for December}
#' }
"rc_makkink"


#' rc_params table
#'
#' This table contains information on attributes of parameters used in rotsee
#'
#' @format A data.table with 23 rows and 13 columns:
#' \describe{
#'  \item{code}{The parameter code}
#'  \item{parameter}{Brief description of the parameter}
#'  \item{unit}{The unit of the parameter if applicable}
#'  \item{product}{Data classifier, A = Soil measurements, B = Environmental characteristics,
#'   D = Soil or feed characteristics derived from soil/feed measurements,
#'   M = Soil management measures, S = Scores, RM = Recommendations (measures/gifts),
#'   I = Indicators, F = Feed measurements, P = soil amendment product}
#'  \item{element}{Indicates the chemical element or parameter name}
#'  \item{method1}{Method used to determine value}
#'  \item{method2}{Additional details on method}
#'  \item{data_type}{Type of data the parameter pertains to: numeric, integer, char, bool, geom, enum}
#'  \item{value_min}{Lowest possible value the parameter may have if numeric or integer}
#'  \item{value_max}{Highest possible value the parameter may have if numeric or integer}
#'  \item{explanation}{Some additional explanation}
#'  \item{enum}{boolean whether parameter values are drawn from a limited set}
#'  \item{options}{Allowed values for a parameter of type enum separated by "||"}
#' }
"rc_params"