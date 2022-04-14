\encoding{UTF-8}
\name{fill.internal.NA}
\alias{fill.internal.NA}
\title{Fill Internal NA}
\description{
  This function fills internal \code{NA} values (i.e., those with numeric data
  above and below a small data gap) in each column of a
  \code{data.frame} such as a data set of ring widths as produced by
  \code{\link{read.rwl}}.
}
\usage{
fill.internal.NA(x, fill = c("Mean", "Spline", "Linear"))
}
\arguments{
  \item{x}{a \code{data.frame} of ring widths with
    \code{row.names(\var{x})} containing years and \code{names(\var{x})}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{fill}{a \code{numeric} value (e.g., \code{0}) or a \code{character} string of
    \code{"Mean"}, \code{"Spline"}, or \code{"Linear"}.  Defaults to \code{"Mean"}.}
}
\details{
  There are occasionally data gaps within a tree-ring series.  Some of
  the functions in dplR will fail
  when an internal \code{NA} is encountered (e.g. \code{\link{caps}}).  This
  function fills internal \code{NA} values with either a given numeric value
  (e.g., \code{0}) or through crude imputation.  The latter can be calculated as 
  the mean of the series (\code{fill="Mean"}) or calculated by fitting a cubic spline
  (\code{fill="Spline"}) using the \code{\link{spline}} function or calculated
  by linear approximation (\code{fill="Linear"}) using the function
  \code{\link{approx}}.

  Editorial: Having internal \code{NA} in a tree-ring series is
  often bad practice and filling those values should be done with
  caution.  For instance, some users code missing rings as \code{NA}
  instead of \code{0}.  And missing values (i.e., \code{NA}) are
  sometimes present in maximum latewood density data when the rings are
  small.  A common, but not recommended, practice is to leave stretches
  of \code{NA} values in places where it has been impossible to
  accurately measure rings (perhaps because of a break in the core).  It
  is often better to treat that core as two separate series (e.g., "01A"
  and "01B" rather than have internal \code{NA} values.  As with all
  processing, the analyst should make a decision based on their
  experience with the wood and not rely on software to make a choice for
  them!
}
\value{
  A \code{data.frame} with \code{colnames(\var{x})} and
  \code{rownames(\var{x})}.  Internal \code{NA}s
  filled as above.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{spline}}, \code{\link{approx}}
}
\examples{
library(graphics)
library(stats)
foo <- data.frame(x1=c(rnorm(5), NA, NA, rnorm(3)),
                  x2=c(rnorm(10)),
                  x3=c(NA, NA, rnorm(3), NA, rnorm(4)),
                  x4=c(NA, NA, rnorm(3), NA, rnorm(3), NA),
                  x5=c(NA, NA, rnorm(8)),
                  x6=c(NA, rnorm(9)),
                  x7=c(NA, rnorm(5), NA, rnorm(3)),
                  x8=c(rnorm(8), NA, NA),
                  x9=c(rnorm(5), NA, rnorm(3), NA))
row.names(foo) <- 1901:1910
class(foo) <- c("rwl","data.frame")
fill.internal.NA(foo, fill=0)

bar <- fill.internal.NA(foo, fill="Spline")
baz <- fill.internal.NA(foo, fill="Linear")

## note differences in method "Spline" vs. "Linear"
yrs <- time(foo)
plot(yrs, foo$x7, type="b", lwd=3)
lines(yrs, bar$x7, col="red", lwd=2)
lines(yrs, baz$x7, col="green", lwd=1)

}
\keyword{ manip }
