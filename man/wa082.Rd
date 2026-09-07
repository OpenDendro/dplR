\encoding{UTF-8}
\name{wa082}
\docType{data}
\alias{wa082}
\title{Hurricane Ridge, Pacific silver fir}
\description{
  This data set gives the raw ring widths for Pacific silver fir
  \emph{Abies amabilis} at Hurricane Ridge in Washington, \acronym{USA}.
  There are 23 series.  Data set was created using \code{\link{read.rwl}}
  and saved to an .rda file using \code{\link{save}}.

  The source file records no measurement for series \code{712011} in the year
  1900, holding the value \code{-999} there.  This data set was built before
  \code{\link{read.tucson}} distinguished a missing measurement from a ring
  width of zero, and that cell holds a zero.  It has been left as it is for
  continuity with earlier versions of dplR.  Reading the source file today
  gives \code{NA} in that cell; passing \code{fill.internal.NA = 0} to
  \code{\link{read.tucson}} reproduces this data set exactly.
}
\usage{data(wa082)}
\format{
  A \code{data.frame} containing 23 tree-ring series in columns and 286
  years in rows.
}
\source{
  International tree-ring data bank, Accessed on 20-April-2021 at
  \url{https://www.ncei.noaa.gov/pub/data/paleo/treering/measurements/northamerica/usa/wa082.rwl}
}
\references{
  Schweingruber, F. (1983) Hurricane Ridge Data Set.  \acronym{IGBP}
  \acronym{PAGES}/World Data Center for Paleoclimatology Data
  Contribution Series 1983-wa082.RWL.  \acronym{NOAA}/\acronym{NCDC}
  Paleoclimatology Program, Boulder, Colorado, \acronym{USA}.
}
\examples{
library(utils)
data(wa082)

## Where the data came from. read.tucson() records what it saw and attaches
## it to what it returns; this data set carries that record.
prov <- attr(wa082, "dplR.provenance")
prov$file
prov$header

## The file records no measurement for series 712011 in 1900, holding -999
## there. This data set was built with fill.internal.NA = 0, so that cell
## holds a zero. See the description above.
prov$gaps
wa082["1900", "712011"]

## rwl.report() shows the same in its header
rwl.report(wa082)
}
\keyword{datasets}
