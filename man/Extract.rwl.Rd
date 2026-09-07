\encoding{UTF-8}
\name{Extract.rwl}
\alias{[.rwl}
\alias{subset.rwl}
\title{ Subset an rwl Object }
\description{
  Subset a \code{rwl} object by series (columns) and years (rows), keeping
  the class and the provenance record of the data.
}
\usage{
\method{[}{rwl}(x, i, j, drop)

\method{subset}{rwl}(x, subset, select, drop = FALSE, \dots)
}
\arguments{

  \item{x}{ an object of class \code{"rwl"} with series as columns and
    years as rows, such as that produced by \code{\link{read.rwl}}. }

  \item{i, j}{ indices for the rows (years) and columns (series), used
    exactly as in \code{\link{[.data.frame}}: numbers, negative numbers,
    logicals, or names.  The year of a row is its name, so
    \code{x["1900", ]} selects a year and \code{x[time(x) \%in\% 1800:1900, ]}
    selects a window of them. }

  \item{drop}{ logical.  As in \code{\link{[.data.frame}}: if the result has
    one column and \code{drop} is \code{TRUE}, a \code{numeric} vector is
    returned instead of a one-series \code{rwl} object.  Note that
    \code{subset} defaults to \code{drop = FALSE}, as
    \code{\link{subset.data.frame}} does. }

  \item{subset, select}{ for the \code{subset} method, expressions naming the
    years (rows) to keep and the series (columns) to keep, used exactly as in
    \code{\link{subset.data.frame}}. }

  \item{\dots}{ Not used. }

}
\details{

  Subsetting an \code{rwl} object works as it does for a
  \code{\link{data.frame}}, with \code{i} indexing years and \code{j}
  indexing series.  Three things are different.

  \emph{Dropping series returns the years the remaining series cover.}  A
  collection is ragged, and the years at the ends of an \code{rwl} object
  usually belong to a few long series, so dropping some of the series
  ordinarily leaves years in which nothing that remains was measured.  The result runs
  from the first year to the last year in which any of the selected series
  has a measurement.  Empty years between those two are kept: an
  \code{rwl} object holds one row per year, and an interior year cannot be
  dropped without breaking the sequence.

  This applies to calls that drop series without indexing years --
  \code{x[, j]}, \code{x[j]}, and \code{subset} given only \code{select}.
  Two kinds of call leave the years alone.  A call that indexes years
  returns exactly the years it asks for, measured or not: \code{x[i, ]},
  \code{x[i, j]}, \code{\link{head}}, \code{\link{tail}},
  \code{\link{common.interval}}, and \code{subset} with its \code{subset}
  argument.  A year window is how an \code{rwl} object is aligned with
  something else, such as a climate series or a second collection, and
  returning fewer years than were asked for would put that alignment out.
  And a call that keeps every series, such as the reordering
  \code{x[, order(names(x))]} or \code{x[]}, has dropped nothing to trim
  for, whatever empty years the object already held.

  Two results are not trimmed.  A subset holding no measurement in any year
  is returned whole rather than emptied, and \code{\link{rwl.check}} reports
  it.  A single series taken with \code{drop = TRUE} is a \code{numeric}
  vector, which carries no years at all, so it is returned at full length
  and stays aligned with the object it came from; use \code{drop = FALSE}
  for a one-series \code{rwl} object trimmed to its own years.

  \emph{Years must stay consecutive.}  An \code{rwl} object holds one row
  per year in increasing order, and dplR reads the year of a measurement
  from its row name, so \code{\link{time.rwl}}, \code{\link{plot.rwl}},
  \code{\link{detrend}}, \code{\link{chron}}, \code{\link{rwl.stats}} and
  the crossdating functions all take the next row to be the next year.  A
  subset such as \code{x[c(1, 5, 9), ]} leaves that untrue.  Where row
  subsetting leaves years that are not consecutive and increasing, because
  rows were taken from the middle, repeated, or reordered, the result is
  returned as a plain \code{data.frame}, with a warning, and without the
  provenance record.  The measurements are unchanged; what is withdrawn is
  the claim that they are a set of dated series.

  \emph{The provenance record is carried and cut to fit.}  The
  \code{"dplR.provenance"} attribute attached by \code{\link{read.tucson}}
  follows the subset.  What describes the file -- its name, the reader, the
  header lines -- is kept.  What describes series -- the precision of each,
  the renames, the interior gaps, the problems found while parsing -- is
  kept for the series that remain, and gaps are dropped where they fall
  outside the years that remain.  \code{mixed.precision} is recomputed,
  since a file measured at two precisions can be subset down to series
  measured at one.  A \code{subset} element is added, holding the series in
  the result, its first and last year, and \code{all.series}, which is
  \code{TRUE} when every series the file held is still present.  A record
  that cannot be matched to a series in the result is dropped rather than
  kept, so an object whose series have been renamed since it was read
  carries a record that says nothing about them in preference to one that
  says something wrong.

  \code{\link{common.interval}} cuts an object down to the years its series
  \emph{share}, which is a different question from the years they cover
  between them.

}

\value{

  An object of class \code{c("rwl", "data.frame")}, or a \code{numeric}
  vector when one column is selected and \code{drop} is \code{TRUE}, or a
  \code{data.frame} when row subsetting has left the years no longer
  consecutive.  When series are dropped and years are not indexed, the
  result runs only over the years the remaining series cover.

}
\author{ Andy Bunn }
\seealso{
  \code{\link{read.rwl}}, \code{\link{read.tucson}}, \code{\link{rwl.check}},
  \code{\link{common.interval}}, \code{\link{time.rwl}},
  \code{\link{subset.data.frame}}
}
\examples{
library(utils)
data(ca533)

## Series are columns and years are rows. Selecting series drops the years
## that none of them covers: ca533 begins in 626, but these five series do
## not.
ca533.sub <- ca533[, 1:5]
class(ca533.sub)
range(time(ca533))
range(time(ca533.sub))

## Naming years returns those years, whether or not they hold data.
dim(ca533[time(ca533) \%in\% 1000:1099, 1:5])

## subset() works the same way.
range(time(subset(ca533, select = 1:5)))

## Years are subset by name or by time().
ca533.1800s <- ca533[time(ca533) \%in\% 1800:1899, ]
range(time(ca533.1800s))

## A single series comes back as a vector unless drop is FALSE.
class(ca533[, 1])
class(ca533[, 1, drop = FALSE])

## The provenance record of the data follows the subset.
data(wa082)
gap.series <- attr(wa082, "dplR.provenance")$gaps$series
wa082.sub <- wa082[, gap.series, drop = FALSE]
attr(wa082.sub, "dplR.provenance")$gaps
attr(wa082.sub, "dplR.provenance")$subset$all.series

## Rows taken out of the middle break the one-row-per-year promise that
## dplR relies on. This warns, and gives back a data.frame.
not.rwl <- ca533[c(1, 5, 9), ]
class(not.rwl)
}
\keyword{ manip }
