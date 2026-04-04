\encoding{UTF-8}
\name{xdate.floater}
\alias{xdate.floater}
\alias{print.floater}
\alias{plot.floater}
\title{Crossdate an undated series}
\description{
  Pulls an undated (or misdated) series through a dated \code{rwl} object
  in order to establish possible dates for the series.
}
\usage{
xdate.floater(rwl, series, series.name = "Unknown", min.overlap = 50,
              n = NULL, prewhiten = TRUE, biweight = TRUE,
              method = c("spearman", "pearson", "kendall"),
              make.plot = TRUE, return.rwl = TRUE, verbose = TRUE)

\method{print}{floater}(x, \dots)

\method{plot}{floater}(x, \dots)
}
\arguments{
  \item{rwl}{a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}}.}
  \item{series}{a numeric vector of ring widths, e.g. a single column
    from an \code{rwl} object.}
  \item{series.name}{a \code{character} string giving a name for the
    undated series. Defaults to \code{"Unknown"}.}
  \item{min.overlap}{a positive integer giving the minimum number of
    years of overlap required between the series and the master
    chronology at each search position. Defaults to \code{50}.}
  \item{n}{\code{NULL} or a positive integer giving the filter length
    for the \code{\link{hanning}} filter used for removal of low
    frequency variation prior to correlation. \code{NULL} skips the
    filter.}
  \item{prewhiten}{logical flag. If \code{TRUE} (the default) each
    series is whitened using \code{\link{ar}} prior to correlation.}
  \item{biweight}{logical flag. If \code{TRUE} (the default) a robust
    mean is used when building the master chronology via
    \code{\link{tbrm}}.}
  \item{method}{the correlation coefficient to use. One of
    \code{"spearman"} (default), \code{"pearson"}, or \code{"kendall"}.
    See \code{\link{cor.test}}.}
  \item{make.plot}{logical flag. If \code{TRUE} (the default) a
    two-panel plot is produced showing the segments and the correlation
    surface. See Details.}
  \item{return.rwl}{logical flag. If \code{TRUE} (the default) a
    \code{"floater"} list object is returned. If \code{FALSE} only the
    correlation statistics \code{data.frame} is returned.}
  \item{verbose}{logical flag. If \code{TRUE} (the default) a brief
    summary is printed to the console.}
  \item{x}{a \code{"floater"} object produced by
    \code{xdate.floater}.}
  \item{\dots}{additional arguments — currently ignored.}
}
\details{
  The undated series is slid along the master chronology built from
  \code{rwl} (using the leave-one-out principle) and the correlation
  between the series and the master is computed at each position with
  sufficient overlap. The position with the highest correlation gives
  the proposed dates.

  Both series and master are optionally prewhitened and/or smoothed
  with a Hanning filter before correlation, consistent with the
  approach used in \code{\link{corr.series.seg}}.

  The two-panel plot (produced when \code{make.plot = TRUE}, or by
  calling \code{plot()} on the returned object) shows: (1) a segment
  plot of the master series with the floater at its best-fit position
  highlighted in green; and (2) the correlation at each end-year
  searched, overlaid with a dashed significance line and a light-blue
  polygon showing the 5th--95th percentile band of the typical
  interseries correlation in the master chronology. The dark-blue
  horizontal line shows the median interseries correlation. The
  best-fit position is marked with green points and a dashed segment.
}
\value{
  If \code{return.rwl = TRUE} (the default), an object of class
  \code{"floater"} is returned. This is a named list with the
  following elements:
  \item{series.name}{the name given to the undated series.}
  \item{floaterCorStats}{a \code{data.frame} with columns \code{first},
    \code{last}, \code{r}, \code{p}, and \code{n} giving the start year,
    end year, correlation, p-value, and sample size at each search
    position.}
  \item{rwlCombined}{an \code{rwl} object containing the dated master
    series combined with the undated series placed at its best-fit
    dates.}
  \item{rwlOut}{an \code{rwl} object containing only the undated series
    at its best-fit dates.}

  If \code{return.rwl = FALSE}, only the \code{floaterCorStats}
  \code{data.frame} is returned.
}
\note{
  This function is experimental and may change in future releases.
  Users should always verify proposed dates visually and against the
  physical wood.

  The choice of \code{min.overlap} affects both which search positions
  are evaluated and the reliability of the correlations at those
  positions. A short \code{min.overlap} allows the series to be placed
  near the edges of the master chronology where little overlap exists,
  producing correlations based on few observations that may be
  spuriously high. A very long \code{min.overlap} restricts the search
  to positions where the series is well inside the master, which is
  statistically conservative but can cause the function to miss the
  correct dates entirely if the true position lies near an edge, or to
  produce wrong dates if the best correlation within the constrained
  search window happens to be a false match. As a rule,
  \code{min.overlap} should be long enough to produce a stable
  correlation (50 years is a common starting point for annual
  tree-ring data) but no longer than roughly half the length of the
  undated series. The dashed significance line in the plot is
  calculated from the overlap length at each position and provides a
  useful guide: positions with short overlaps will have high
  significance thresholds and should be interpreted with caution.
}
\author{Andy Bunn. Patched and improved by Mikko Korpela.}
\seealso{
  \code{\link{corr.series.seg}}, \code{\link{ccf.series.rwl}},
  \code{\link{skel.plot}}, \code{\link{series.rwl.plot}}
}
\examples{
library(utils)
data(co021)
summary(co021)

# Remove a series and try to recover its dates
foo <- co021[, "645232"]
bar <- co021
bar$"645232" <- NULL
out <- xdate.floater(bar, foo, min.overlap = 50, series.name = "645232")
print(out)

# A longer series. With min.overlap = 100 the correct dates are recovered.
# With min.overlap = 200 the search window is so constrained that the true
# position is excluded and the function returns the best match within the
# remaining positions, which is a false fit. Compare the two results.
foo <- co021[, "646118"]
bar <- co021
bar$"646118" <- NULL
out <- xdate.floater(bar, foo, min.overlap = 100, series.name = "646118")
out <- xdate.floater(bar, foo, min.overlap = 200, series.name = "646118")
}
\keyword{manip}
