\encoding{UTF-8}
\name{window.rwl}
\alias{window.rwl}
\alias{window.rwi}
\alias{window.crn}
\alias{window.bai}
\title{ Take a Span of Years }
\description{
  Take the years from \code{start} to \code{end} of an \code{"rwl"},
  \code{"rwi"}, \code{"bai"} or \code{"crn"} object.
}
\usage{
\method{window}{rwl}(x, start = NULL, end = NULL, \dots)

\method{window}{rwi}(x, start = NULL, end = NULL, \dots)

\method{window}{bai}(x, start = NULL, end = NULL, \dots)

\method{window}{crn}(x, start = NULL, end = NULL, \dots)
}
\arguments{
  \item{x}{ an object of class \code{"rwl"}, \code{"rwi"}, \code{"bai"} or
    \code{"crn"}. }
  \item{start, end}{ the first and last years to keep, each a single
    number.  \code{NULL} means the first or last year of \code{x}. }
  \item{\dots}{ not used. }
}
\details{
  The years are those in \code{time(x)}.  The rows are taken with
  \code{[}, so an \code{"rwl"}, \code{"rwi"} or \code{"bai"} object keeps its class
  and its provenance record, cut down to the years that remain (see
  \code{\link{[.rwl}}).  Series with no values in the window are
  dropped from an \code{"rwl"}, \code{"rwi"} or \code{"bai"} object, with a message
  naming them, since an empty series is not one any dplR function can
  use; if no series has values in the window, that is an error.  The
  years returned are still the ones asked for.  \code{subset} drops
  empty series in the same way.  \code{x[i, ]} does not: \code{[}
  returns every series it is asked for, empty or not, so that its
  columns stay lined up with anything indexed alongside them (see
  \code{\link{[.rwl}}).  Use \code{window} to take years for analysis.

  A window is usually taken to line the data up with something else, so
  a window that runs past either end of \code{x} gives a warning that
  says which years were returned, and a window that does not overlap
  \code{x} at all is an error.
}
\value{
  An object of the same class as \code{x}, holding the years from
  \code{start} to \code{end} and the series with values in them.
}
\author{ Andy Bunn }
\seealso{ \code{\link{time.rwl}}, \code{\link{[.rwl}},
  \code{\link[stats]{window}} }
\examples{
data(ca533)
ca533.1800s <- window(ca533, 1800, 1899)
range(time(ca533.1800s))
ca533.crn <- chron(detrend(ca533, method = "Spline"))
tail(window(ca533.crn, start = 1950))
}
\keyword{ manip }
