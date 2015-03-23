\encoding{UTF-8}
\name{ccf.series.rwl}
\alias{ccf.series.rwl}
\title{ Cross-Correlation between a Series and a Master Chronology }
\description{
  Computes cross-correlations between a tree-ring series and a master
  chronology built from a rwl object at user-specified lags and segments.
}
\usage{
ccf.series.rwl(rwl, series, series.yrs = as.numeric(names(series)),
               seg.length = 50, bin.floor = 100, n = NULL,
               prewhiten = TRUE, biweight = TRUE, pcrit = 0.05,
               lag.max = 5, make.plot = TRUE,
               floor.plus1 = FALSE, \dots)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as rows
    such as that produced by \code{\link{read.rwl}}. }
  \item{series}{ a \code{numeric} or \code{character} vector.  Usually a
    tree-ring series.  If the length of the value is 1, the
    corresponding column of \code{\var{rwl}} is selected (by name or
    position) as the series and ignored when building the master
    chronology.  Otherwise, the value must be \code{numeric}. }
  \item{series.yrs}{ a \code{numeric} vector giving the years of
    \code{\var{series}}.  Defaults to
    \code{as.numeric(names(\var{series}))}.  Ignored if
    \code{\var{series}} is an index to a column of \code{\var{rwl}}. }
  \item{seg.length}{ an even integral value giving length of segments in
    years (e.g., 20, 50, 100 years). }
  \item{bin.floor}{ a non-negative integral value giving the base for
    locating the first segment (e.g., 1600, 1700, 1800
    \acronym{AD}).  Typically 0, 10, 50, 100, etc. }
  \item{n}{ \code{NULL} or an integral value giving the filter length
    for the \code{\link{hanning}} filter used for removal of low
    frequency variation. }
  \item{prewhiten}{ \code{logical} flag.  If \code{TRUE} each series is
    whitened using \code{\link{ar}}. }
  \item{biweight}{ \code{logical} flag.  If \code{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}}. }
  \item{pcrit}{ a number between 0 and 1 giving the critical value for the
    correlation test. }
  \item{lag.max}{ an integral value giving the maximum lag at which to
    calculate the \code{\link{ccf}}. }
  \item{make.plot}{ \code{logical} flag indicating whether to make a
    plot. }
  \item{floor.plus1}{ \code{logical} flag.  If \code{TRUE}, one year is
    added to the base location of the first segment (e.g., 1601, 1701,
    1801 \acronym{AD}). }
  \item{\dots}{ other arguments passed to plot. }
}
\details{
  This function calculates the cross-correlation function between a
  tree-ring series and a master chronology built from \code{\var{rwl}}
  looking at correlations lagged positively and negatively using
  \code{\link{ccf}} at overlapping segments set by
  \code{\var{seg.length}}.  For instance, with \code{\var{lag.max}} set
  to 5, cross-correlations would be calculated at for each segment with
  the master lagged at \code{\var{k} = -5:5} years.
  
  The cross correlations are calculated calling
  \code{\link{ccf}} as \cr
  \code{ccf(x=series, y=master, lag.max=lag.max, plot=FALSE)}.  Note that
  prior to dplR version 1.60, the \code{master} was set as \code{x} and 
  the \code{series} as \code{y}.  This was changed to be more in line with 
  user expectations so that a missing ring in a series produces a positive
  lag in the plot rather than a negative lag.  This structure of this call
  does put the plots at odds with Figure 3 in Bunn (2010) which is unfortunate.

  Correlations are calculated for the first segment, then the
  second segment and so on.  Correlations are only calculated for segments with
  complete overlap with the master chronology.

  Each series (including those in the \code{\var{rwl}} object) is
  optionally detrended as the residuals from a \code{\link{hanning}}
  filter with weight \code{\var{n}}.  The filter is not applied if
  \code{\var{n}} is \code{NULL}.  Detrending can also be done via
  prewhitening where the residuals of an \code{\link{ar}} model are
  added to each series mean.  This is the default.  The master chronology
  is computed as the mean of the \code{\var{rwl}} object using
  \code{\link{tbrm}} if \code{\var{biweight}} is \code{TRUE} and
  \code{rowMeans} if not.  Note that detrending typically changes the
  length of the series. E.g., a \code{\link{hanning}} filter will
  shorten the series on either end by \code{floor(\var{n}/2)}.  The
  prewhitening default will change the series length based on the
  \code{\link{ar}} model fit.  The effects of detrending can be seen with
  \code{\link{series.rwl.plot}}.

}
\value{
  A \code{list} containing matrices \code{\var{ccf}} and
  \code{\var{bins}}.  Matrix \code{\var{ccf}} contains the correlations
  between the series and the master chronology at the lags window given
  by \code{\var{lag.max}}.  Matrix \code{\var{bins}} contains the years
  encapsulated by each bin.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{corr.rwl.seg}}, \code{\link{corr.series.seg}},
  \code{\link{skel.plot}}, \code{\link{series.rwl.plot}}
}
\references{ Bunn, A. G. (2010) Statistical and visual crossdating 
  in R using the dplR library.  \emph{Dendrochronologia},
  \bold{28}(4), 251\enc{–}{--}258.
}
\examples{
library(utils)
data(co021)
dat <- co021
## Create a missing ring by deleting a year of growth in a random series
flagged <- dat$"641143"
flagged <- c(NA, flagged[-325])
names(flagged) <- rownames(dat)
dat$"641143" <- NULL
ccf.100 <- ccf.series.rwl(rwl = dat, series = flagged, seg.length = 100)
\dontrun{
flagged2 <- co021$"641143"
names(flagged2) <- rownames(dat)
ccf.100.1 <- ccf.series.rwl(rwl = dat, seg.length = 100,
                            series = flagged2)
## Select series by name or column position
ccf.100.2 <- ccf.series.rwl(rwl = co021, seg.length = 100,
                            series = "641143")
ccf.100.3 <- ccf.series.rwl(rwl = co021, seg.length = 100,
                            series = which(colnames(co021) == "641143"))
identical(ccf.100.1, ccf.100.2) # TRUE
identical(ccf.100.2, ccf.100.3) # TRUE
}
}
\keyword{ manip }
