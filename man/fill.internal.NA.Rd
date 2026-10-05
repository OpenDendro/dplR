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
fill.internal.NA(x, fill = c("Mean", "Spline", "Linear", "Chron"),
                 series = names(x), nyrs = 20, min.series = 3)
}
\arguments{
  \item{x}{a \code{data.frame} of ring widths with
    \code{row.names(\var{x})} containing years and \code{names(\var{x})}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{fill}{a \code{numeric} value (e.g., \code{0}) or a \code{character} string of
    \code{"Mean"}, \code{"Spline"}, \code{"Linear"}, or \code{"Chron"}.
    Defaults to \code{"Mean"}.}
  \item{series}{a \code{character} vector naming the series to fill.
    Defaults to every series in \code{\var{x}}.  Other series are
    returned unchanged.}
  \item{nyrs}{a number greater than 1.  Used only by
    \code{\var{fill} = "Chron"}: the stiffness, in years, of the spline
    (see \code{\link{caps}}) fitted to each series as its growth curve.}
  \item{min.series}{a number, 1 or greater.  Used only by
    \code{\var{fill} = "Chron"}: how many other series must have a
    measurement in a year for that year to be filled.}
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
  \code{\link{approx}}.  Those three look only at the series being
  filled.  \code{fill="Chron"} takes the value from the other series
  in \code{\var{x}} and is described in its own section below.

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
\section{Filling from the other series (\code{fill = "Chron"})}{
  A mean, a line or a spline through one series can say what size of
  ring to expect in a gap, but nothing about whether the missing years
  were wide or narrow ones.  The other trees at the site can.
  \code{\var{fill} = "Chron"} estimates each missing ring from them.

  The method is in the spirit of the one in the program
  \acronym{ARSTAN} (Cook and Krusic), whose subroutine \code{fillin}
  fills every gap in a data set before detrending.  It follows the same
  outline:
  \enumerate{
    \item A spline of \code{\var{nyrs}} years (\code{\link{caps}}) is
    fitted to every series in \code{\var{x}} and each series is divided
    by its spline, giving indices.  The default of 20 years is
    \acronym{ARSTAN}'s.
    \item For the series being filled, the indices of all the
    \emph{other} series are averaged year by year with a robust mean
    (\code{\link{tbrm}}).
    \item In each gap year, the departure of that mean from its own
    average is expressed in standard deviations, and then given the
    mean and standard deviation of the indices of the series being
    filled.
    \item The result is multiplied by the spline of the series being
    filled, to return to ring width.  An estimate below zero is set to
    zero.
  }

  It is not a port of \code{fillin} and will not reproduce
  \acronym{ARSTAN}'s numbers.  The differences are deliberate:
  \itemize{
    \item \acronym{ARSTAN} fits a weighted spline that ignores the gap
    years.  Here the gap is first bridged with a straight line and the
    spline is fitted to the bridged series.
    \item \acronym{ARSTAN}'s mean includes the series being filled.
    Here it is left out.
    \item \acronym{ARSTAN} detrends the mean a second time and
    stabilises its variance.  Neither is done here.
    \item \acronym{ARSTAN} fills from a single series if that is all
    there is and leaves the value missing if there is none.  Here a
    year with fewer than \code{\var{min.series}} other series stops
    the call.
    \item \acronym{ARSTAN} treats any negative value as a gap, and
    zero as well when the data are transformed.  Here only \code{NA}
    is a gap.
  }

  Assumptions:
  \itemize{
    \item The data are ring widths or another quantity for which a
    ratio to a growth curve makes sense: no negative values, and a
    curve that stays above zero.
    \item The series share a common year-to-year signal.  The method
    assumes the series being filled would have followed the others in
    the years it is missing.  Where the series do not crossdate, the
    filled values are noise with a convincing shape.
    \item The series are correctly dated.  A dating error in the
    series being filled, or in several of the others, puts the wrong
    year's signal in the gap.
    \item The gap is short relative to \code{\var{nyrs}}.  The
    growth curve across a gap comes from a straight line between the
    rings either side of it, so over a long gap the level of the filled
    values is a guess, whatever their year-to-year pattern.
  }

  Limitations:
  \itemize{
    \item The filled values are estimates, not measurements, and
    nothing in the returned object marks which they are.  Keep the
    unfilled data.
    \item The fill is circular.  The values come from the other series
    and then sit beside them, so statistics that measure agreement
    among series (\code{\link{rwi.stats}},
    \code{\link{interseries.cor}}, \code{\link{corr.rwl.seg}})
    are biased upwards in the filled years, and sample depth counts
    series that were not measured there.  Do not crossdate with filled
    values.
    \item The filled values are given the full year-to-year variance
    of the series, as if it tracked the others perfectly.  That keeps
    the variance of the series from dropping in the gap, at the cost
    of filled values that vary more than a regression estimate would.
    \item An estimate that falls below zero is returned as zero, with
    a warning naming the series and years.  A zero ring width reads as
    a locally absent ring everywhere else in dplR.
    \item A year can be filled only if at least
    \code{\var{min.series}} other series were measured in it.  If any
    gap year falls short, the call stops, names the series and years,
    and fills nothing.  With \code{\var{min.series}} of 1 or 2 the
    \dQuote{mean} is one tree or the average of two.
    \item The series being filled and the mean of the others need at
    least three years in common to set the scale, and a scale set from
    a few years is poor.  Short series are filled badly.
    \item A series with fewer than three measurements, or whose spline
    reaches zero, has no usable growth curve.  If it is one of the
    series to be filled the call stops.  If it is one of the others it
    is left out of the mean without comment.
    \item Gaps are not filled from each other: a year that is missing
    in several series is estimated for each from the series that have
    it.
    \item Filled values do not fall on the measurement grid of the
    data (0.01 or 0.001 mm).  They are not rounded.
  }
}
\value{
  \code{\var{x}}, with the internal \code{NA}s of the series named in
  \code{\var{series}} filled as above.  It keeps the class and attributes
  of \code{\var{x}}: an \code{rwl} object comes back as an \code{rwl}
  object, with the record \code{\link{read.tucson}} attached to it.
  Before dplR 1.8.1 the result was a plain \code{data.frame}.
}
\references{
  Cook, E. R. and Krusic, P. J.  Program \acronym{ARSTAN}: a tree-ring
  standardization program based on detrending and autoregressive time
  series modeling, with interactive graphics.  Tree-Ring Laboratory,
  Lamont-Doherty Earth Observatory of Columbia University.  The
  \code{"Chron"} method follows the outline of subroutine
  \code{fillin} in version 49v1 of the Fortran source.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{spline}}, \code{\link{approx}},
  \code{\link{caps}}, \code{\link{tbrm}}
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

## fill = "Chron" uses the other series. Remove ten rings from one
## series of co021 and compare the estimates with what was measured.
data(co021)
gap <- as.character(1801:1810)
dat <- co021
dat[gap, "641114"] <- NA
chr <- fill.internal.NA(dat, fill="Chron", series="641114")
lin <- fill.internal.NA(dat, fill="Linear", series="641114")
plot(1801:1810, co021[gap, "641114"], type="b", lwd=3,
     xlab="Year", ylab="Ring width (mm)")
lines(1801:1810, chr[gap, "641114"], col="red", lwd=2)
lines(1801:1810, lin[gap, "641114"], col="green", lwd=1)

}
\keyword{ manip }
