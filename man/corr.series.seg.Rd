\name{corr.series.seg}
\alias{corr.series.seg}
\title{ Compute Correlation between a Series and a Master Chronology }
\description{
  Compute correlation between a tree-ring series and a master chronology by
  segment.
}
\usage{
  corr.series.seg(rwl,series,series.yrs=as.numeric(names(series)),
  seg.length=50,bin.floor=100,n=NULL, prewhiten = TRUE, biweight=TRUE,
  pcrit=0.05, make.plot = TRUE,...)
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
  \item{pcrit}{ a number between 0 and 1 giving the p-value to calculate
    the critical value for the cross correlation. }
  \item{make.plot}{ logical flag indicating whether to make a plot. }
  \item{...}{ other arguments passed to \code{\link{plot}}. }
}
\details{
  This function calculates correlations between each a tree-ring series and
  a master chronology built from all series in a rwl object. Correlations are done
  by segment where segments are lagged by half the segment length (e.g., 100-year
  segments overlapped by 50-years). Correlations are calculcated for the first
  segment, then the second segment and so on. Correlations are only calculated
  for segments with complete overlap with the master chronology. A moving correlation
  with length equal to the segment length is also calcualted between the series
  and the master.

  The function is typically invoked to produce a plot showing the correlation
  for each segment and the moving correlation.

  Each series is optionally detrended as the residuals from a
  \code{\link{hanning}} filter with weight \code{n}. The filter is not applied
  if \code{n} is \code{NULL}. Detrending can optionally be done with prewhitening where the residuals of
  an \code{\link[stats]{ar}} model are added to each series mean.
  This is the default. The master chronology is computed as the mean of
  rwl object using \code{\link{tbrm}} if \code{biweight=TRUE} and \code{rowMeans}
  if not. Note that detrending can change the length of the series. E.g., a
  \code{\link{hanning}} filter will shorten the series on either end by
  \code{floor(n/2)}. The effects of detrending can be seen with
  \code{\link{series.rwl.plot}}.

}
\value{
  A \code{list} containing matrices \code{bins}, \code{moving.rho}, and vectors
  \code{spearman.rho}, \code{p.val}, and \code{overall}.
}
\author{ Andy Bunn }
\seealso{ \code{\link{corr.series.seg}} \code{\link{skel.plot}}  \code{\link{series.rwl.plot}} \code{\link{ccf.series.rwl}}}
\examples{
  data(co021)
  dat=co021
  #create a missing ring by deleting a year of growth in a random series
  flagged=dat$'641143'
  flagged=c(NA,flagged[-325])
  names(flagged)=rownames(dat)
  dat$'641143'=NULL
  seg.100=corr.series.seg(rwl=dat,series=flagged,seg.length=100,biweight=FALSE)
}
\keyword{ manip }

