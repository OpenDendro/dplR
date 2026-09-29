\encoding{UTF-8}
\name{as.rwi}
\alias{as.rwi}
\alias{rwi}
\alias{[.rwi}
\alias{subset.rwi}
\alias{time.rwi}
\alias{time<-.rwi}
\alias{summary.rwi}
\alias{print.summary.rwi}
\alias{as.data.frame.summary.rwi}
\alias{plot.rwi}
\title{ Ring-Width Index (rwi) Objects }
\description{
  Class \code{"rwi"} holds ring-width indices: detrended series with one
  series per column and one year per row, as made by
  \code{\link{detrend}}, \code{\link{rcs}} and \code{\link{cms}}.
  \code{as.rwi} labels a \code{data.frame} or \code{matrix} of indices
  as one.
}
\usage{
as.rwi(x)

\method{[}{rwi}(x, i, j, drop)

\method{subset}{rwi}(x, subset, select, drop = FALSE, \dots)

\method{time}{rwi}(x, \dots)

\method{summary}{rwi}(object, ids = NULL, pcrit = 0.05, \dots)

\method{print}{summary.rwi}(x, max.print = 10, \dots)

\method{as.data.frame}{summary.rwi}(x, \dots)

\method{plot}{rwi}(x, plot.type = c("spag", "seg", "image"), \dots)
}
\arguments{
  \item{x, object}{ for \code{as.rwi}, a \code{data.frame} or
    \code{matrix} with series as columns and years as row names; for the
    methods, an object of class \code{"rwi"}, or of class
    \code{"summary.rwi"} for \code{print} and \code{as.data.frame}. }
  \item{ids}{ an optional \code{data.frame} grouping the series by tree,
    with columns \code{tree} and \code{core}, one row per series, as for
    \code{\link{rwi.stats}} (see \code{\link{read.ids}}).  \code{NULL}
    counts each series as its own tree. }
  \item{pcrit}{ the significance level below which a series is taken to
    correlate with the others. }
  \item{max.print}{ the most series to list as not correlating. }
  \item{i, j, subset, select, drop}{ as for \code{\link{[.rwl}} and
    \code{\link{subset.rwl}}. }
  \item{plot.type}{ \code{"spag"} for \code{\link{spag.plot}},
    \code{"seg"} for \code{\link{seg.plot}}, or \code{"image"} for a
    heat map of the indices.  See Details. }
  \item{\dots}{ for \code{plot}, passed to the plotting function; for
    \code{plot.type = "image"} this includes \code{clip}, the quantile
    at which the colours are clipped (default 0.99), and arguments to
    \code{\link{image}}.  Otherwise not used. }
}
\details{
  An \code{"rwi"} object has the same shape as an \code{"rwl"} object
  and makes the same promise: the row names are the years, consecutive
  and increasing.  It does not inherit from \code{"rwl"}, so that
  indices and ring widths can be told apart.

  dplR's functions use the class to check what they are given.  Those
  that want ring widths -- \code{\link{detrend}}, \code{\link{rcs}},
  \code{\link{cms}}, \code{\link{i.detrend}}, \code{\link{bai.in}},
  \code{\link{bai.out}}, \code{\link{pointer}},
  \code{\link{strip.rwl}}, \code{\link{ssf}} and
  \code{\link{rwl.report}} -- warn when given an \code{"rwi"} object,
  and say what goes wrong: \code{detrend} on indices, for instance,
  divides out a growth curve that has already been removed.  Those that
  want indices -- \code{\link{chron}}, \code{\link{chron.ars}},
  \code{\link{chron.stabilized}}, \code{\link{rwi.stats}},
  \code{\link{rwi.stats.running}} and \code{\link{sss}} -- warn when
  given an \code{"rwl"} object: \code{rwi.stats(ca533)} gives an
  \code{rbar.eff} of 0.350 from the widths against 0.423 from the
  Spline indices, and nothing about the number says it is wrong.  They
  still take a plain \code{data.frame} or matrix without a word.  The
  crossdating functions, the plots, \code{\link{common.interval}},
  \code{\link{sgc}} and \code{\link{rwl.stats}} take either class
  quietly.  Each warning is only a warning, and the function goes on:
  if the class is what is wrong -- indices read from a file with
  \code{\link{read.rwl}} come back as class \code{"rwl"} -- relabel
  them with \code{as.rwi}, or relabel widths with \code{\link{as.rwl}}.

  Two attributes travel with it.  \code{attr(x, "dplR.detrend")} is a
  list that says how the indices were made: the function
  (\code{fun}), the method and its settings, and always
  \code{difference}, which is \code{TRUE} when the indices are
  differences from the fitted curve (centred on 0) rather than ratios
  (centred on 1).  \code{attr(x, "dplR.provenance")} is the provenance
  record of the ring widths the indices were made from, if they had one
  (see \code{\link{read.tucson}}).  Neither is set by \code{as.rwi},
  which only labels the object.

  Subsetting works as for \code{"rwl"} objects (\code{\link{[.rwl}}):
  the class and both records are kept, dropping series trims the years
  that none of the remaining series cover, and a row subset that leaves
  the years out of order or with holes returns a plain
  \code{data.frame}, with a warning.  \code{subset} and
  \code{\link{window.rwi}} drop series left with no values and name
  them in a message; \code{[} keeps them, on purpose, so that its
  columns stay lined up with anything indexed alongside them.

  \code{summary} describes the indices as a collection.  It gives how
  they were made (from \code{attr(x, "dplR.detrend")}), their span and
  common interval, the collection statistics from
  \code{\link{rwi.stats}} (including rbar.eff, EPS and SNR), and for
  each series its first and last years, length, mean, standard
  deviation, first-order autocorrelation, and its correlation with the
  mean of the other series, with the p-value, from
  \code{\link{interseries.cor}} with its defaults.  Printing it shows
  the collection and lists the series that do not correlate with the
  others at \code{pcrit}; \code{as.data.frame} gives the per-series
  table.  The collection statistics count each series as its own tree
  unless \code{ids} is given, and the print says which.  The print ends
  by pointing to \code{summary} with \code{ids} (when it was not
  given),
  \code{\link{rwi.stats.running}} (rbar and EPS through time, which
  usually fall off where sample depth does) and
  \code{\link{corr.rwl.seg}} (where in a series the fit breaks down).
  A series with no values at all is listed and left out of the
  collection statistics, the correlations and the common interval.  With
  fewer than two series with values there is no collection, and the
  statistics and correlations are \code{NULL} and \code{NA}.

  \code{plot} draws, by default, \code{\link{spag.plot}}, which draws
  indices around 1 (or 0 for differences) and not around each series'
  mean, so the grey line under each series is the value the indices
  should sit at.  \code{plot.type = "image"} draws each index as a
  coloured cell, with years across and series down, earliest-starting
  at the bottom.  Colours are brown below 1 (or 0) and green above it.
  Each side is scaled separately and clipped at the \code{clip}
  quantile of its departures, since ratio indices cannot fall below 0
  but can run well above 2; the key gives the clips.  A run of one
  colour at the start or end of a series is a growth trend the
  detrending did not remove, and a vertical stripe is a year the series
  agree on.

  Arithmetic on an \code{"rwi"} object (e.g. \code{x + 1}) returns a
  plain \code{data.frame}, as it does for any \code{data.frame}
  subclass.  Use \code{as.rwi} on the result if it is still a set of
  indices.
}
\value{
  \code{as.rwi}: an object of class \code{c("rwi", "data.frame")}.
  \code{x} is returned unchanged if it already has that class.
  \code{time}: a \code{numeric} vector of years.
  \code{summary}: an object of class \code{"summary.rwi"}, a list with
  \code{how} (the \code{dplR.detrend} record), \code{n.series},
  \code{first}, \code{last}, \code{common} (the first and last years
  in which every series has a value, or \code{NA}), \code{stats} (from
  \code{rwi.stats}), \code{ids.given}, \code{empty} (the names of series with no values),
  \code{series} (the per-series \code{data.frame})
  and \code{pcrit}.
}
\author{ Andy Bunn }
\seealso{ \code{\link{detrend}}, \code{\link{rcs}}, \code{\link{cms}},
  \code{\link{as.rwl}}, \code{\link{chron}}, \code{\link{rwi.stats}} }
\examples{
library(utils)
data(ca533)
ca533.rwi <- detrend(ca533, method = "Spline")
class(ca533.rwi)
str(attr(ca533.rwi, "dplR.detrend"))
summary(ca533.rwi)
## Group cores by tree, from the series IDs
summary(ca533.rwi, ids = autoread.ids(ca533))
head(as.data.frame(summary(ca533.rwi)))
plot(ca533.rwi[, 1:5])
data(co021)
## The growth trend "Mean" leaves in: a run of green at the start of
## nearly every series.
plot(detrend(co021, method = "Mean"), plot.type = "image")
}
\keyword{ manip }
\keyword{ classes }
