<!-- badges: start -->
[![R-CMD-check](https://github.com/ricrossi/combiroc/workflows/R-CMD-check/badge.svg)](https://github.com/ricrossi/combiroc/actions)
<!-- badges: end -->

# CombiROC v.1.0

CombiROC is an R package for the combinatorial study of multimarkers panels. 

The CombiROC tool was first published by [Mazzara et al. Scientific Reports 2017](https://www.nature.com/articles/srep45477). A description of the analitycal protocol is also published in [Bombaci & Rossi, Methods Mol Biol 2019](https://link.springer.com/protocol/10.1007%2F978-1-4939-9164-8_16).
The web-app Shiny version of CombiROC is still available at [combiroc.eu](http://combiroc.eu/): this Shiny version appeared first but has limited features and is not further maintained. For full capabilities use this pacakge.

The code in this repo is work in progress, and is uploaded here "as-is" with no warranties implied. Improvements and new features will be added on a regular basis, please check on this github page for new features and releases. 

A preprint :page\_facing\_up: on CombiROC package is now available on bioRxiv at <https://www.biorxiv.org/content/#>.

## Installation

```r
# First, install devtools from CRAN
install.packages("devtools")

# Then install the development version of CombiROC from GitHub
library(devtools)
install_github("ricrossi/combiroc")
```
## Documentation and tutorials

Main documentation is in the package's vignette. You can also find the web version of the documentation at the project website <https://ricrossi.github.io/combiroc>, created with `pkgdown`.

## Contributors

* Package authors and curators: Ivan Ferrari, Riccardo L. Rossi
* Initial code and Shiny App author: Saveria Mazzara

