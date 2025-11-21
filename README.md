# rotsee

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check](https://github.com/AgroCares/rotsee/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/AgroCares/rotsee/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

> [!NOTE]
> The `rotsee` package is currently under active development and is not ready to be used. The functionality may change, and the results should be considered preliminary.

## Introduction

`rotsee` is an R package providing an implementation of the Rothamsted Carbon Model (RothC) for agricultural soils. RothC is a model used to simulate organic carbon turnover in top soils, considering factors like soil properties, crop rotation, amendment application, and climate.

This package serves as a RothC calculation core, to enable accurate and fast calculations. The package is equipped with easy-to-use functions to prepare input and to perform the calculations.

## Installation

You can install the development version of rotsee from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("AgroCares/rotsee")
```

## Usage

A basic example of how to use the package:

```r
library(rotsee)
library(data.table)

example_crop <- data.table(
 B_LU_START = c("2020-01-01", "2020-06-01"),
 B_LU_END = c("2020-03-31", "2020-08-31"),
 B_LU_HC = c(0.5, 0.3),
 B_C_OF_INPUT = c(100, 200)
 )
 
 example_amendment <- data.table(
 P_HC = c(0.5, 0.3),
 P_DATE_FERTILIZATION = c("2020-01-01", "2020-06-01"),
 B_C_OF_INPUT = c(10000, 8000)
 )
 
 example_soil <- data.table(
 A_CLAY_MI = c(10),
 A_SOM_LOI = c(8),
 A_DENSITY_SA = c(1.47)
 )
 
 example_result <- rc_sim(soil_properties = example_soil,
 rothc_rotation = example_crop,
 rothc_amendment = example_amendment)
 
 print(example_result)

```

## License

This project is licensed under the GPL-3 License - see the [LICENSE](LICENSE) file for details.

## Acknowledgment

The `rotsee` package is developed and maintained by the [Nutrienten Management Instituut (NMI)](https://nmi-agro.nl) in Wageningen, the Netherlands.

![Logo of NMI](https://media.licdn.com/dms/image/C560BAQEYGcm4HjNnxA/company-logo_200_200/0?e=2159024400&v=beta&t=u40rJ7bixPWB2SAqaj3KCKzJRoKcqf0wUXCdmsTDQvw)

## Contact

For questions, bug reports, and suggestions, please use the [issue tracker](https://github.com/AgroCares/rotsee/issues).