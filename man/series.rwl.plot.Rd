\name{series.rwl.plot}
\alias{series.rwl.plot}
\title{ Compute Correlation between a Series and a Master Chronology }
\description{
  Compute correlation between a tree-ring series and a master chronology by
  segment.
}
\usage{
  series.rwl.plot(rwl,series,series.yrs=as.numeric(names(series)),
  seg.length=100,bin.floor=100,n=NULL, prewhiten = TRUE, biweight=TRUE, ...)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as rows
    such as that produced by \code{\link{read.rwl}}. }
  \item{series}{ a numeric vector. Usually a tree-ring series. }
  \item{series.yrs}{ a numeric vector giving the years of the series.
    Defaults to \code{as.numeric(names(series))}. }
  \item{seg.length}{ an integer giving length of segments in years. }
  \item{bin.floor}{ an integer giving base for locating the first segment
    (e.g.,.1600, 1700, 1800 AD). Typically 0, 10, 50, 100, etc. }
  \item{n}{ \code{NULL} or an integer giving the filter length for the
    \code{\link{hanning}} filter used for removal of low frequency
    variation. }
  \item{prewhiten}{ logical flag. If TRUE each series is whitened using
      \code{\link[stats]{ar}} prior to averaging. }
  \item{biweight}{logical flag. If TRUE then a robust mean is calculated
    using \code{\link{tbrm}.}}
  \item{...}{ other arguments passed to \code{\link{plot}}. }
}
\details{
  The function is typically invoked to produce a plot showing a time series
  plot of the series and the master as well as a scatterplot of series vs.
  master. The series and master are returned as well.

  Each series is optionally detrended as the residuals from a
  \code{\link{hanning}} filter with weight \code{n}. The filter is not applied
  if \code{n} is \code{NULL}. Detrending can optionally be done with prewhitening where the residuals of
  an \code{\link[stats]{ar}} model are added to each series mean.
  This is the default. The master chronology is computed as the mean of
  rwl object using \code{\link{tbrm}} if \code{biweight=TRUE} and \code{rowMeans}
  if not. Note that detrending can change the length of the series. E.g., a
  \code{\link{hanning}} filter will shorten the series on either end by
  \code{floor(n/2)}.

}
\value{
  A \code{list} containing the filtered vectors \code{series} and \code{master}.
}
\author{ Andy Bunn }
\seealso{ \code{\link{corr.rwl.seg}} \code{\link{corr.series.seg}} \code{\link{skel.plot}} \code{\link{ccf.series.rwl}}}
\examples{
  data(co021)
  dat=co021
  flagged=dat$'646244'
  names(flagged)=rownames(dat)
  dat$'646107'=NULL
  foo=series.rwl.plot(rwl=dat,series=flagged,seg.length=100,n=5)
  #note effect of n on first year in the series
  foo=series.rwl.plot(rwl=dat,series=flagged,seg.length=100,n=13,prewhiten=FALSE)
  bar=series.rwl.plot(rwl=dat,series=flagged,seg.length=100,n=7,prewhiten=FALSE)
  head(foo$series)
  head(bar$series)
}
\keyword{ manip }

