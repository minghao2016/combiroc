<img src="inst/www/combiroc.png" align="right" alt="" width="120" />

<!-- badges: start -->
[![R-CMD-check](https://github.com/ingmbioinfo/combiroc/workflows/R-CMD-check/badge.svg)](https://github.com/ingmbioinfo/combiroc/actions)
[![Lifecycle:experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental-1)
[![](https://img.shields.io/github/last-commit/ingmbioinfo/combiroc.svg)](https://github.com/ingmbioinfo/combiroc/commits/master)
![GitHub all releases](https://img.shields.io/github/downloads/ingmbioinfo/combiroc/total)
<!-- badges: end -->

# CombiROC

CombiROC is a totally new music in multi-markers analysis: an R package for efficient and easy combinatorial selection of biomarkers and sensitivity/specificity-driven prioritization of features. 

This is the development version of CombiROC package (combiroc), code in this repo is work in progress and it is uploaded here "as-is" with no warranties implied. Improvements and new features will be added on a regular basis, please check on this github page for new features and releases. 

The CombiROC approach was first published by [Mazzara et al. Scientific Reports 2017](https://www.nature.com/articles/srep45477). A description of the analitycal protocol is also published in [Bombaci & Rossi, Methods Mol Biol 2019](https://link.springer.com/protocol/10.1007%2F978-1-4939-9164-8_16).
The web-app Shiny version of CombiROC is still available at [combiroc.eu](http://combiroc.eu/), but it has limited features (as well as low computational power) and is not further maintained. For full capabilities and customized analyses is advisable to install this package on your own machines.

## Installation

```r
# First, install devtools from CRAN and run it
install.packages("devtools")
library(devtools)

# Then install the development version of CombiROC from GitHub
install_github("ingmbioinfo/combiroc")
```

## Quick start example (for full documentation see further)

```r
library(combiroc)

# load the preformatted demo dataset
data <- demo_data

# shape it in long format (prone to plotting)
data_long <- combiroc_long(data)

# study distribution of you markers' signal
# adjust lims and cutoffs according to your own data
distr <- markers_distribution(data_long, case_class = 'A', 
	y_lim = 0.0015, x_lim = 3000, 
	signalthr_prediction = TRUE, 
	min_SE = 40, min_SP = 80, 
	boxplot_lim = 2000)

# explore the distr object: boxplot of signals
distr$Boxplot

# explore the distr object: densities of classes with signal threshold (signalthr)
distr$Density_plot
distr$Density_summary

# explore the distr object: ROC and its coordinates
distr$ROC
head(distr$Coord, n=10)

# combinatorial analysis
tab <- combi(data, signalthr = 407, combithr = 1)

# SE & SP computation
mks <- se_sp(data, tab)

# ranked combinations
rmks <- ranked_combs(data, mks, case_class = 'A', min_SE = 40, min_SP = 80)

# check ranked combinations
rmks$table
rmks$bubble_chart

# results report for specific markers/combinations
reports <-roc_reports(data, markers_table = tab, case_class = 'A',
                      single_markers =c('Marker1'), 
                      selected_combinations = c(11,15))

# results outputs
reports$Plot
reports$Metrics
```

## Documentation and tutorials

Main documentation is in the package's vignette. You can also find the web version of the documentation at the [combiroc-package website](https://ingmbioinfo.github.io/combiroc/index.html) created with `pkgdown`.

## Contributors

* Package authors and maintainers: Ivan Ferrari & Riccardo L. Rossi
* Original code of Shiny App: Saveria Mazzara
* Initial idea & conception: Mauro Bombaci

