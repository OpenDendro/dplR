
# dplR <img src="https://github.com/AndyBunn/dplR/blob/master/dplR_Sticker.png" width="220" align="right" />

<!-- badges: start -->
#### Release Version
[![CRAN_version_badge](https://www.r-pkg.org/badges/version/dplR)](https://cran.r-project.org/package=dplR)
[![CRAN_last_release_badge](https://www.r-pkg.org/badges/last-release/dplR)](https://cran.r-project.org/package=dplR)
[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/last-month/dplR?color=blue)](https://r-pkg.org/pkg/dplR)
#### Development Version
[![R-CMD-check](https://github.com/andybunn/dplR/workflows/R-CMD-check/badge.svg)](https://github.com/andybunn/dplR/actions)
[![dplR status badge](https://andybunn.r-universe.dev/badges/dplR)](https://andybunn.r-universe.dev)

<!-- badges: end -->
  
## Overview

`dplR` is a package for R that performs tree-ring analyses such as detrending, chronology building, and crossdating. `dplR` is part of `OpenDendro` which has Python implementations of many of the `dplR` functions. 

## Installation

### CRAN

The latest, stable, release version of `dplR` can be installed from [The Comprehensive R Archive Network (CRAN)](https://cran.r-project.org/) from the R prompt:

```R
install.packages("dplR")
```

### Develoment Version

CRAN releases of dplR are relatively infrequent. However, the development version of `dplR` is available here as source code and as binaries. Most users who want the development version are best off getting it via `r-universe` from the R prompt:

```R
install.packages("dplR", repos = "https://andybunn.r-universe.dev/")
```

Alternatively, the development version can be installed from the source code with the `devtools` package.

```R
devtools::install_github("andybunn/dplR")
```

This should be straightforward for Linux users (if any still exist) but because `dplR` includes both C and Fortran code, you will need the appropriate compilers installed on your system to build from source. Configuring these is relatively easy for Mac and less so for Windows. Unless there is a good reason to want to build from source (e.g., you are a geek or a developer) it's more typical to install a binary.

## Getting Started

New users of dplR can begin by working with the introductory chapters in [Learning to Love dplR](https://opendendro.github.io/dplR-workshop/) which contains instructional material for using `dplR`.

## Interactive Apps

Some dendro tasks might be done more easily interactively. As such, we have two modest apps using Shiny and `dplR` as the back-end.

* Statistical crossdating: [`xDater`](https://andybunn.shinyapps.io/xDateR/)
* Interactive detrending: [`iDetrend`](https://andybunn.shinyapps.io/iDetrend/)


