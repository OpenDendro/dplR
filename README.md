
# dplR <img src="https://github.com/AndyBunn/dplR/blob/master/dplR_Sticker.png" width="220" align="right" />

<!-- badges: start -->
[![CRAN_version_badge](https://www.r-pkg.org/badges/version/dplR)](https://cran.r-project.org/package=dplR)
[![CRAN_last_release_badge](https://www.r-pkg.org/badges/last-release/dplR)](https://cran.r-project.org/package=dplR)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/last-month/dplR?color=blue)](https://r-pkg.org/pkg/dplR)
[![R-CMD-check](https://github.com/andybunn/dplR/workflows/R-CMD-check/badge.svg)](https://github.com/andybunn/dplR/actions)
[![dplR status badge](https://andybunn.r-universe.dev/badges/dplR)](https://andybunn.r-universe.dev)

<!-- badges: end -->
  
## Overview

Perform tree-ring analyses such as detrending, chronology building, and crossdating. Read and write standard file formats used in dendrochronology.

## Installation

### CRAN

The latest, stable, release version of `dplR` can be installed from [The Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/) as follows:

```R
install.packages("dplR")
```

This is the officially released version of `dplR`.

### Develoment Version
The code here on GitHub is the version of `dplR` that is currently being developed. It might be unstable.

You can install the development version using `r-universe`.

```R
install.packages("dplR", repos = "https://andybunn.r-universe.dev/")
```

Alternatively, the development version can be installed from the source code with the `devtools` package.

```R
devtools::install_github("andybunn/dplR")
```

Because `dplR` includes both C and Fortran code, you will need the appropriate compilers installed on your system to build from source.

## Getting Started

New users of dplR can begin by working with the introductory chapters in [Learning to Love dplR](https://opendendro.github.io/dplR-workshop/) which contains instructional material for using `dplR`.

Users interested in crossdating should visit the [`xDater` app](https://andybunn.shinyapps.io/xDateR/). 
