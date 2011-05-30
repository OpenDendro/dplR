\name{corr.rwl.seg}
\alias{corr.rwl.seg}
\title{ Compute Correlations between Series }
\description{
  Computes the correlation between each tree-ring seriesin a rwl object.
}
\usage{
  corr.rwl.seg(rwl,seg.length=50,bin.floor=100,n=NULL, prewhiten = TRUE,
  pcrit=0.05, biweight=TRUE, make.plot = TRUE,...)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as rows
    such as that produced by \code{\link{read.rwl}}. }
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
  This function calculates correlation serially between each tree-ring series and
  a master chronology built from all series in a rwl object. Correlations are done
  by segment where segments are lagged by half the segment length (e.g., 100-year
  segments overlapped by 50-years). Correlations are calculcated for the first
  segment, then the second segment and so on. Correlations are only calculated for segments with complete overlap
  with the master chronology. Each series is optionally detrended as the residuals from a
  \code{\link{hanning}} filter with weight \code{n}. The filter is not applied
  if \code{n} is \code{NULL}. Detrending can optionally be done with prewhitening where the residuals of
  an \code{\link[stats]{ar}} model are added to each series mean.
  This is the default. The master chronology is computed as the mean of
  rwl object using \code{\link{tbrm}} if \code{biweight=TRUE} and \code{rowMeans}
  if not. Note that detrending can change the length of the series. E.g., a
  \code{\link{hanning}} filter will shorten the series on either end by
  \code{floor(n/2)}. The effects of detrending can be seen with
  \code{\link{series.rwl.plot}}. The function is typically invoked to produce a
  plot where each segment for each series is colored by its correlation to the
  master chronology. Green segments are those that do not overlap completely
  with the width of the bin. Blue segments are those that correlate above the
  user-specified critical value. Red segments are those that correlate below the
  user-specified critical value and might indicate a dating problem. NOTE: this
  function is slow for large rwl objects and will remain so until a C or
  Fortran loop is coded to perform the correlations.
}
\value{
  A \code{list} containing matrices \code{spearman.rho}, \code{p.val},
  \code{overall}, \code{bins}, vector \code{avg.seg.rho}. An additional
  character \code{flags} is also returned if any segments fall below the critical
  value. Matrix \code{spearman.rho} contains the correlations each series by bin.
  Matrix \code{p.val} contains the p-values on the correlation for each series
  by bin. Matrix \code{overall} contains the average correlation and p-value for
  each series. Matrix \code{bins} contains the years encapsulated by each bin.
  The vector \code{avg.seg.rho} contains the average correlation for each bin.
}
\author{ Andy Bunn }
\seealso{ \code{\link{corr.series.seg}} \code{\link{skel.plot}}  \code{\link{series.rwl.plot}} \code{\link{ccf.series.rwl}}}

\examples{
  data(co021)
  corr.rwl.seg(co021,seg.length=100)
}
\keyword{ manip }

