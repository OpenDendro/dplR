\encoding{UTF-8}
\name{ca533}
\docType{data}
\alias{ca533}
\title{Campito Mountain Tree Ring Widths}
\description{
  This data set gives the raw ring widths for bristlecone pine
  \emph{Pinus longaeva} at Campito Mountain in California,
  \acronym{USA}.  There are 34 series.  Data set was created using
  \code{\link{read.rwl}} and saved to an .rda file using
  \code{\link{save}}.
}
\usage{data(ca533)}
\format{
  A \code{data.frame} containing 34 tree-ring series in columns and 1358
  years in rows.
}
\source{
  International tree-ring data bank, Accessed on 20-April-2021 at
  \url{https://www.ncei.noaa.gov/pub/data/paleo/treering/measurements/northamerica/usa/ca533.rwl}
}
\references{
  Graybill, D. A. and LaMarche, Jr., V. C. (1983) Campito Mountain Data
  Set.  \acronym{IGBP} \acronym{PAGES}/World Data Center for
  Paleoclimatology Data Contribution Series 1983-CA533.RWL.
  \acronym{NOAA}/\acronym{NCDC} Paleoclimatology Program, Boulder,
  Colorado, \acronym{USA}.
}
\examples{
library(utils)
data(ca533)

## Where the data came from. read.tucson() records what it saw and attaches
## it to what it returns; this data set carries that record.
prov <- attr(ca533, "dplR.provenance")
prov$file          # the archived file it was read from
prov$precision     # the precision each series was measured at
prov$gaps          # interior gaps, if any -- this file has none
prov$events        # problems found while reading -- none here either

## rwl.report() puts the file and precision at the head of its report
rwl.report(ca533)
}
\keyword{datasets}
