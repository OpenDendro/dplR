
# dplR <img src="https://github.com/AndyBunn/dplR/blob/master/dplR_Sticker.png" width="220" align="right" />

[![CRAN_version_badge](https://www.r-pkg.org/badges/version/dplR)](https://cran.r-project.org/package=dplR)

[![CRAN RStudio mirror downloads](https://cranlogs.r-pkg.org/badges/last-month/dplR?color=blue)](https://r-pkg.org/pkg/dplR)
## Overview

Perform tree-ring analyses such as detrending, chronology building,
and crossdating. Read and write standard file formats used in
dendrochronology.

## Installation

The latest release version of dplR is installed from CRAN as follows:

```R
install.packages("dplR")
```

The development version can be installed with devtools
(`install.packages("devtools")`):

```R
devtools::install_github("andybunn/dplR")
```

## Vignettes

There are several vignettes for working with dplR. These cover common data task like I/O, detrending, chronology building, some time-series analysis and so on.

```R
browseVignettes(package = "dplR")
```

Users interested in crossdating should visit the [`xDater` app](https://andybunn.shinyapps.io/xDateR/). 
