\name{series.rwl.plot}
\alias{series.rwl.plot}
\title{ Plot Series and a Master  }
\description{
  Plots a tree-ring series with a master chronology and displays
  their fit, segments, and detrending options in support of the
  cross-dating functions.
}
\usage{
  series.rwl.plot(rwl,series,series.yrs=as.numeric(names(series)),
  seg.length=100,bin.floor=100,n=NULL, prewhiten = TRUE, biweight=TRUE)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as rows
    such as that produced by \code{\link{read.rwl}}. }
  \item{series}{ a \code{numeric} vector. Usually a tree-ring series. }
  \item{series.yrs}{ a \code{numeric} vector giving the years of \code{series}.
    Defaults to \code{as.numeric(names(series))}. }
  \item{seg.length}{ an even \code{integer} giving length of segments in years
     (e.g., 20, 50, 100 years). }
  \item{bin.floor}{ a positive \code{integer} giving  the base for locating
    the first segment (e.g.,1600, 1700, 1800 AD). Typically 0, 10, 50, 100,
    etc. }
  \item{n}{ \code{NULL} or an integer giving the filter length for the
    \code{\link{hanning}} filter used for removal of low frequency
    variation. }
  \item{prewhiten}{ \code{logical} flag. If \code{TRUE} each series is whitened using
      \code{\link[stats]{ar}}. }
  \item{biweight}{ \code{logical} flag. If \code{TRUE} then a robust mean is calculated
    using \code{\link{tbrm}}. }
}
\details{
  The function is typically invoked to produce a four plots showing the
  the effect of the detrending options \code{n} and \code{prewhiten} and
  the the binning options \code{seg.length} and \code{bin.floor}.

  Plot 1. Time series plot of the filtered series and the master.
  Plot 2. Scatterplot of series vs. master.
  Plot 3. Segments that would be used in the other cross-dating
  functions (e.g., \code{\link{corr.series.seg}}).
  Plot 4. Text giving the detrending options and the time span of the
  raw and filtered series and master.

  The series and master are returned as well.

  See help pages for  \code{\link{corr.rwl.seg}},
  \code{\link{corr.series.seg}}, and  \code{\link{ccf.series.rwl}} for
  moreinformation on these arguments.

}
\value{
  A \code{list} containing the filtered vectors \code{series} and \code{master}.
}
\author{ Andy Bunn }
\seealso{ \code{\link{corr.rwl.seg}} \code{\link{corr.series.seg}} \code{\link{ccf.series.rwl}}}
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
