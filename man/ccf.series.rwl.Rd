\name{ccf.series.rwl}
\alias{ccf.series.rwl}
\title{ Compute Cross-Correlation between a Series and a Master Chronology }
\description{
  Computes cross-correlations between a tree-ring series and a master
  chronology built from a rwl object at user-specified lags and segments.
}
\usage{
ccf.series.rwl(rwl, series, series.yrs = as.numeric(names(series)),
               seg.length = 50, bin.floor = 100, n = NULL,
               prewhiten = TRUE, biweight = TRUE, pcrit = 0.05,
               lag.max = 5, make.plot = TRUE, ...)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as rows
    such as that produced by \code{\link{read.rwl}}. }
  \item{series}{ a \code{numeric} vector. Usually a tree-ring series. }
  \item{series.yrs}{ a \code{numeric} vector giving the years of \code{series}.
    Defaults to \code{as.numeric(names(series))}. }
  \item{seg.length}{ an even \code{integer} giving length of segments in years
    (e.g., 20, 50, 100 years). }
  \item{bin.floor}{ a non-negative \code{integer} giving the base for
    locating the first segment (e.g.,1600, 1700, 1800 AD). Typically 0,
    10, 50, 100, etc. }
  \item{n}{ \code{NULL} or an integer giving the filter length for the
    \code{\link{hanning}} filter used for removal of low frequency
    variation. }
  \item{prewhiten}{ \code{logical} flag. If \code{TRUE} each series is whitened using
      \code{\link[stats]{ar}}. }
  \item{biweight}{ \code{logical} flag. If \code{TRUE} then a robust mean is calculated
    using \code{\link{tbrm}}. }
  \item{pcrit}{ a number between 0 and 1 giving the critical value for the
    correlation test. }
  \item{lag.max}{ an integer giving the maximum lag at which to calculate
    the \code{\link[stats]{ccf}}. }
  \item{make.plot}{ \code{logical flag} indicating whether to make a plot. }
  \item{...}{ other arguments passed to plot. }
}
\details{
  This function calculates the cross-correlation function between a tree-ring
  series and a master chronology built from a rwl object looking at
  correlations lagged positively and negatively using \code{link{ccf}} at
  overlapping segments set by \code{seg.length}. For instance,
  with \code{lag.max} set to 5, cross-correlations would be
  calculated at for each segment with the master lagged at \code{k=c(-5:5)} years.
  The function is typically invoked to produce a plot.

  Correlations are calculated for the first segment, then the
  second segment and so on. Correlations are only calculated for segments with
  complete overlap with the master chronology.

  Each series (including those in the rwl object) is optionally detrended as the residuals
  from a \code{\link{hanning}} filter with weight \code{n}. The filter is not applied
  if \code{n} is \code{NULL}. Detrending can also be done via prewhitening where
  the residuals of an \code{\link[stats]{ar}} model are added to each series
  mean. This is the default. The master chronology is computed as the mean of
  rwl object using \code{\link{tbrm}} if \code{biweight=TRUE} and \code{rowMeans}
  if not. Note that detrending typically changes the length of the series. E.g., a
  \code{\link{hanning}} filter will shorten the series on either end by
  \code{floor(n/2)}. The prewhitening default will change the series
  length based on the  \code{\link[stats]{ar}} model fit. The effects of
  detrending can be seen with \code{\link{series.rwl.plot}}.

}
\value{
  A \code{list} containing matrices \code{ccf} and \code{bins}. Matrix \code{ccf}
  contains the correlations between the series and the master chronology at
  the lags window given by \code{lag.max}. Matrix \code{bins} contains the
  years encapsulated by each bin.

}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{corr.rwl.seg}}, \code{\link{corr.series.seg}},
  \code{\link{skel.plot}}, \code{\link{series.rwl.plot}}
}
\examples{
data(co021)
dat <- co021
## Create a missing ring by deleting a year of growth in a random series
flagged <- dat$"641143"
flagged <- c(NA, flagged[-325])
names(flagged) <- rownames(dat)
dat$"641143" <- NULL
ccf.100 <- ccf.series.rwl(rwl=dat, series=flagged, seg.length=100)
}
\keyword{ manip }
