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
  pcrit=0.05, make.plot = TRUE, floor.plus1 = FALSE, ...)
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
  \item{make.plot}{ \code{logical flag} indicating whether to make a plot. }
  \item{floor.plus1}{ \code{logical flag}. If \code{TRUE}, one year is
    added to the base location of the first segment (e.g. 1601, 1701,
    1801 AD). }
  \item{...}{ other arguments passed to plot. }
}
\details{
  This function calculates the correlation a tree-ring series and a master
  chronology built from a rwl object. Correlations are done by segment
  (see below) and with a moving correlation with length equal to the
  \code{seg.length}.  The function is typically invoked to produce a plot.



}
\value{
  A \code{list} containing matrices \code{bins}, \code{moving.rho}, and vectors
  \code{spearman.rho}, \code{p.val}, and \code{overall}.

  Matrix \code{bins} contains the years encapsulated by each bin
  (segments). Matrix  \code{moving.rho} contains the moving correlation
  and p-value for a moving average equal to \code{seg.length}.
  Vector \code{spearman.rho} contains the correlations each series by
  bin and \code{p.val} contains the p-values. Vector \code{overall}
  contains the average correlation and p-value.
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{corr.series.seg}} \code{\link{skel.plot}}  \code{\link{series.rwl.plot}} \code{\link{ccf.series.rwl}}}
\examples{
data(co021)
dat <- co021
## Create a missing ring by deleting a year of growth in a random series
flagged <- dat$"641143"
flagged <- c(NA, flagged[-325])
names(flagged) <- rownames(dat)
dat$"641143" <- NULL
seg.100 <- corr.series.seg(rwl=dat, series=flagged, seg.length=100,
                           biweight=FALSE)
}
\keyword{ manip }
