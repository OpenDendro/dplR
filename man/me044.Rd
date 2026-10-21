\encoding{UTF-8}
\name{me044}
\docType{data}
\alias{me044}
\title{Howland Forest, Eastern Hemlock}
\description{
  This data set gives the raw ring widths for eastern hemlock
  \emph{Tsuga canadensis} at Howland Forest in Maine, \acronym{USA}.
  There are 42 series from 21 trees, two cores per tree, measured to
  0.001 mm and spanning 1780 to 2012.  It was read from the
  \acronym{ITRDB} file with \code{\link{read.tucson}} and its default
  settings, and saved to an .rda file using \code{\link{save}}.

  The file has a full three-line \acronym{ITRDB} header, so this data set
  is a good one for seeing what \code{\link{read.tucson}} records about a
  file: see \code{\link{rwl.meta}}.  It has 29 zero (missing) rings in
  17 series and no interior gaps.

  Many of the trees grow much faster from the 1910s on.  In both the
  1910s and the 1920s at least 20 of the 42 series have a ten-year
  growth change of 100\% or more (the mean of the ten years after a year
  against the mean of the ten years before it, after Nowacki and Abrams
  1997).  The example below repeats the calculation.  The trees were
  sampled for studies of climate and carbon isotopes; the collection
  says nothing about the disturbance history of the stand.
}
\usage{data(me044)}
\format{
  A \code{data.frame} of class \code{"rwl"} containing 42 tree-ring
  series in columns and 233 years in rows.  It carries the
  \code{"dplR.provenance"} record from \code{\link{read.tucson}}.
}
\source{
  International tree-ring data bank, file last modified 8 January 2021,
  accessed in 2026 at
  \url{https://www.ncei.noaa.gov/pub/data/paleo/treering/measurements/northamerica/usa/me044.rwl}

  Maxwell, R. S., Belmecheri, S. and Taylor, A. H.  Howland Forest
  (\acronym{ITRDB} ME044).  \acronym{NOAA} National Centers for
  Environmental Information.  DOI: https://doi.org/10.25921/gn7w-y829
}
\references{
  Maxwell, R. S., Belmecheri, S., Taylor, A. H., Davis, K. J. and
  Ocheltree, T. W. (2020) Carbon isotope ratios in tree rings respond
  differently to climatic variations than tree-ring width in a mesic
  temperate forest.  \emph{Agricultural and Forest Meteorology},
  \bold{288--289}, 108014.  DOI: https://doi.org/10.1016/j.agrformet.2020.108014

  Belmecheri, S., Maxwell, R. S., Taylor, A. H., Davis, K. J.,
  Guerrieri, R., Moore, D. J. and Rayback, S. A. (2021) Precipitation
  alters the CO2 effect on water-use efficiency of temperate forests.
  \emph{Global Change Biology}, \bold{27}(8), 1560--1571.
  DOI: https://doi.org/10.1111/gcb.15491

  Nowacki, G. J. and Abrams, M. D. (1997) Radial-growth averaging
  criteria for reconstructing disturbance histories from presettlement-
  origin oaks.  \emph{Ecological Monographs}, \bold{67}(2), 225--249.
}
\examples{
data(me044)

## What the object knows about the file it came from
rwl.meta(me044)

## Ten-year percent growth change for each series: the mean of the ten
## years from a year on, against the mean of the ten years before it.
pgc <- function(x, w = 10) {
  ok <- which(!is.na(x))
  out <- rep(NA_real_, length(x))
  if (length(ok) < 2 * w + 1) return(out)
  y <- x[ok]
  i <- (w + 1):(length(y) - w + 1)
  before <- sapply(i, function(j) mean(y[(j - w):(j - 1)]))
  after <- sapply(i, function(j) mean(y[j:(j + w - 1)]))
  out[ok[i]] <- (after - before) / before * 100
  out
}
g <- sapply(me044, pgc)

## How many series double their growth at some point in each decade
decade <- floor(time(me044) / 10) * 10
sapply(split(as.data.frame(g >= 100), decade),
       function(d) sum(colSums(d, na.rm = TRUE) > 0))
}
\keyword{datasets}
