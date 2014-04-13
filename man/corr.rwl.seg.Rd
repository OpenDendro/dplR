\name{corr.rwl.seg}
\alias{corr.rwl.seg}
\title{ Compute Correlations between Series }
\description{
  Computes the correlation between each tree-ring series in a rwl object.
}
\usage{
corr.rwl.seg(rwl, seg.length = 50, bin.floor = 100, n = NULL,
             prewhiten = TRUE, pcrit = 0.05, biweight = TRUE,
             method = c("spearman", "pearson","kendall"),
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL,
             master.yrs = as.numeric(if (is.null(dim(master))) {
                              names(master)
                          } else {
                              rownames(master)
                          }),
             \dots)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with series as columns and years as
    rows such as that produced by \code{\link{read.rwl}}. }
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
  \item{pcrit}{ a number between 0 and 1 giving the critical value for
    the correlation test. }
  \item{biweight}{ \code{logical} flag.  If \code{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}}. }
  \item{method}{Can either "pearson", "kendall", or "spearman" which indicates
    the correlation coefficient is to be used. Defaults to "spearman." See 
    \code{\link{cor.test}}. }
  \item{make.plot}{ \code{logical flag} indicating whether to make a
    plot. }
  \item{label.cex}{ \code{numeric} scalar for the series labels on the
    plot.  Passed to \code{\var{axis.cex}} in \code{\link{axis}}. }
  \item{floor.plus1}{ \code{logical} flag.  If \code{TRUE}, one year is
    added to the base location of the first segment (e.g., 1601, 1701,
    1801 \acronym{AD}). }
  \item{master}{ \code{NULL}, a \code{numeric} \code{vector} or a
    \code{matrix}-like object of \code{numeric} values, including a
    \code{data.frame}.  If \code{NULL}, a number of master chronologies,
    one for each series in \code{\var{rwl}}, is built from
    \code{\var{rwl}} using the leave-one-out principle.  If a
    \code{vector}, the function uses this as the master chronology.  If
    a \code{matrix} or \code{data.frame}, this object is used for
    building the master chronology (no leave-one-out). }
  \item{master.yrs}{ a \code{numeric} vector giving the years of
    \code{\var{series}}.  Defaults to \code{names} or \code{rownames} of 
    \code{\var{master}} coerced to \code{numeric} type. }
  \item{\dots}{ other arguments passed to plot. }
}
\details{

  This function calculates correlation serially between each tree-ring
  series and a master chronology built from all the other series in the
  \code{\var{rwl}} object (leave-one-out principle).  Optionally, the
  user may give a \code{\var{master}} chronology (a \code{vector}) as an
  argument.  In the latter case, the same master chronology is used for
  all the series in the \code{\var{rwl}} object.  The user can also
  choose to give a \code{\var{master}} \code{data.frame} (series as
  columns, years as rows), from which a single master chronology is
  built.

  Correlations are done for each segment of the series where segments
  are lagged by half the segment length (e.g., 100-year segments would
  be overlapped by 50-years).  The first segment is placed according to
  \code{\var{bin.floor}}.  The minimum bin year is calculated as
  \code{ceiling(\var{min.yr}/\var{bin.floor})*\var{bin.floor}} where
  \code{\var{min.yr}} is the first year in either the \code{\var{rwl}}
  object or the user-specified \code{\var{master}} chronology, whichever
  is smaller.  For example if the first year is 626 and
  \code{\var{bin.floor}} is 100 then the first bin would start in 700.
  If \code{\var{bin.floor}} is 10 then the first bin would start in 630.

  Correlations are calculated for the first segment, then the second
  segment and so on.  Correlations are only calculated for segments with
  complete overlap with the master chronology.  For now, correlations are
  Spearman's rho calculated via \code{\link{cor.test}} using
  \code{method = "spearman"}.

  Each series (including those in the rwl object) is optionally
  detrended as the residuals from a \code{\link{hanning}} filter with
  weight \code{\var{n}}.  The filter is not applied if \code{\var{n}} is
  \code{NULL}.  Detrending can also be done via prewhitening where the
  residuals of an \code{\link{ar}} model are added to each series
  mean.  This is the default.  The master chronology is computed as the
  mean of the \code{\var{rwl}} object using \code{\link{tbrm}} if
  \code{\var{biweight}} is \code{TRUE} and \code{rowMeans} if not.  Note
  that detrending can change the length of the series.  E.g., a
  \code{\link{hanning}} filter will shorten the series on either end by
  \code{floor(\var{n}/2)}.  The prewhitening default will change the
  series length based on the \code{\link{ar}} model fit.  The effects of
  detrending can be seen with \code{\link{series.rwl.plot}}.

  The function is typically invoked to produce a plot where each segment
  for each series is colored by its correlation to the master
  chronology.  Green segments are those that do not overlap completely
  with the width of the bin.  Blue segments are those that correlate
  above the user-specified critical value.  Red segments are those that
  correlate below the user-specified critical value and might indicate a
  dating problem.
}
\value{
  A \code{list} containing matrices \code{\var{spearman.rho}},
  \code{\var{p.val}}, \code{\var{overall}}, \code{\var{bins}}, vector
  \code{\var{avg.seg.rho}}.  An additional \code{character}
  \code{\var{flags}} is also returned if any segments fall below the
  critical value.  Matrix \code{\var{spearman.rho}} contains the
  correlations for each series by bin.  Matrix \code{\var{p.val}}
  contains the p-values on the correlation for each series by
  bin.  Matrix \code{\var{overall}} contains the average correlation and
  p-value for each series. Matrix \code{\var{bins}} contains the years
  encapsulated by each bin.  The vector \code{\var{avg.seg.rho}}
  contains the average correlation for each bin.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{
  \code{\link{corr.series.seg}}, \code{\link{skel.plot}},
  \code{\link{series.rwl.plot}}, \code{\link{ccf.series.rwl}}
}
\examples{data(co021)
corr.rwl.seg(co021, seg.length = 100, label.cex = 1.25)
}
\keyword{ manip }

