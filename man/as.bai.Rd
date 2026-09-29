\encoding{UTF-8}
\name{as.bai}
\alias{as.bai}
\alias{bai}
\alias{[.bai}
\alias{subset.bai}
\alias{time.bai}
\alias{time<-.bai}
\alias{summary.bai}
\alias{plot.bai}
\title{ Basal Area Increment (bai) Objects }
\description{
  Class \code{"bai"} holds basal area increment: the area of each ring,
  with one series per column and one year per row, as made by
  \code{\link{bai.in}} and \code{\link{bai.out}}.  \code{as.bai} labels
  a \code{data.frame} or \code{matrix} of ring areas as one.
}
\usage{
as.bai(x)

\method{[}{bai}(x, i, j, drop)

\method{subset}{bai}(x, subset, select, drop = FALSE, \dots)

\method{time}{bai}(x, \dots)

\method{summary}{bai}(object, \dots)

\method{plot}{bai}(x, plot.type = c("seg", "spag"), \dots)
}
\arguments{
  \item{x, object}{ for \code{as.bai}, a \code{data.frame} or
    \code{matrix} with series as columns and years as row names; for the
    methods, an object of class \code{"bai"}. }
  \item{i, j, subset, select, drop}{ as for \code{\link{[.rwl}} and
    \code{\link{subset.rwl}}. }
  \item{plot.type}{ \code{"seg"} for \code{\link{seg.plot}} or
    \code{"spag"} for \code{\link{spag.plot}}. }
  \item{\dots}{ for \code{plot}, passed to the plotting function.
    Otherwise not used. }
}
\details{
  A \code{"bai"} object has the same shape as an \code{"rwl"} object and
  makes the same promise: the row names are the years, consecutive and
  increasing.  It inherits from neither \code{"rwl"} nor \code{"rwi"},
  so that areas, widths and indices can be told apart.

  dplR's functions use the class to check what they are given.  Those
  that want ring-width indices, such as \code{\link{chron}} and
  \code{\link{rwi.stats}}, take a \code{"bai"} object without a word:
  a mean of basal area increment by year is a \acronym{BAI} chronology.
  So do the crossdating functions, the plots,
  \code{\link{common.interval}}, and \code{\link{detrend}}, since
  fitting a curve to basal area increment is established practice.
  Functions that want ring widths, such as \code{\link{bai.in}},
  \code{\link{rcs}}, \code{\link{cms}} and \code{\link{rwl.report}},
  warn and treat the areas as widths.  If the values really are widths,
  relabel them with \code{\link{as.rwl}}.

  Two attributes travel with it.  \code{attr(x, "dplR.bai")} says how
  the areas were made: the function (\code{fun}) and whether the
  distance to pith (\code{d2pith}, for \code{bai.in}) or the diameters
  (\code{diam}, for \code{bai.out}) were given rather than estimated
  from the widths.  \code{attr(x, "dplR.provenance")} is the provenance
  record of the ring widths the areas were made from, if they had one
  (see \code{\link{read.tucson}}).  Neither is set by \code{as.bai},
  which only labels the object.

  Subsetting works as for \code{"rwl"} objects (\code{\link{[.rwl}}),
  and \code{\link{window}} takes a span of years
  (\code{\link{window.rwl}}).  \code{summary} gives the per-series
  statistics of \code{\link{rwl.stats}}, in the units of the areas.
}
\value{
  \code{as.bai}: an object of class \code{c("bai", "data.frame")}.
  \code{x} is returned unchanged if it already has that class.
  \code{time}: a \code{numeric} vector of years.
  \code{summary}: a \code{data.frame}, as from \code{\link{rwl.stats}}.
}
\author{ Andy Bunn }
\seealso{ \code{\link{bai.in}}, \code{\link{bai.out}},
  \code{\link{as.rwl}}, \code{\link{as.rwi}}, \code{\link{chron}} }
\examples{
library(utils)
data(gp.rwl)
data(gp.d2pith)
gp.bai <- bai.in(rwl = gp.rwl, d2pith = gp.d2pith)
class(gp.bai)
str(attr(gp.bai, "dplR.bai"))
head(summary(gp.bai))
## A BAI chronology
gp.bai.crn <- chron(gp.bai)
plot(gp.bai.crn, add.spline = TRUE, nyrs = 32)
plot(gp.bai[, 1:5])
}
\keyword{ manip }
\keyword{ classes }
