\encoding{UTF-8}
\name{co021}
\docType{data}
\alias{co021}
\title{Schulman Old Tree No. 1, Mesa Verde}
\description{
  This data set gives the raw ring widths for Douglas fir
  \emph{Pseudotsuga menziesii} at Mesa Verde in Colorado, \acronym{USA}.
  There are 35 series.  Data set was created using \code{\link{read.rwl}}
  and saved to an .rda file using \code{\link{save}}.
}
\usage{data(co021)}
\format{
  A \code{data.frame} containing 35 tree-ring series in columns and 788
  years in rows.
}
\source{
  International tree-ring data bank, Accessed on 20-April-2021 at
  \url{https://www.ncei.noaa.gov/pub/data/paleo/treering/measurements/northamerica/usa/co021.rwl}
}
\references{
  Schulman, E. (1963) Schulman Old Tree No. 1 Data Set.  \acronym{IGBP}
  \acronym{PAGES}/World Data Center for Paleoclimatology Data
  Contribution Series 1983-CO021.RWL.  \acronym{NOAA}/\acronym{NCDC}
  Paleoclimatology Program, Boulder, Colorado, \acronym{USA}.
}
\examples{
library(utils)
data(co021)

## Where the data came from. read.tucson() records what it saw and attaches
## it to what it returns; this data set carries that record.
prov <- attr(co021, "dplR.provenance")
prov$file          # the archived file it was read from
prov$precision     # the precision each series was measured at
prov$gaps          # interior gaps, if any -- this file has none
prov$events        # problems found while reading -- none here either

## rwl.report() puts the file and precision at the head of its report
rwl.report(co021)
}
\keyword{datasets}
