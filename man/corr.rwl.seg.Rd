\encoding{UTF-8}
\name{corr.rwl.seg}
\alias{corr.rwl.seg}
\title{ Compute Correlations between Series }
\description{
  Computes the correlation between each tree-ring series in a rwl object.
}
\usage{
corr.rwl.seg(rwl, seg.length = 50, bin.floor = 100, n = NULL,
             nyrs = NULL, prewhiten = TRUE, ar.order.max = NULL,
             pcrit = 0.05, biweight = TRUE,
             method = c("spearman", "pearson","kendall"),
             make.plot = TRUE, label.cex = 1, floor.plus1 = FALSE,
             master = NULL, lag.max = 0,
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
  \item{nyrs}{ \code{NULL} or a number greater than zero.  If not
    \code{NULL}, each series is divided by a smoothing spline with this
    rigidity (see \code{\link{caps}}) to remove low-frequency variation,
    as \code{\link{detrend}} does with \code{method = "Spline"}.  A value
    of 1 or less is taken as a proportion of each series' length.
    Unlike the \code{\link{hanning}} filter, the spline removes no years
    from the ends of a series.  Cannot be combined with \code{\var{n}}. }
  \item{prewhiten}{ \code{logical} flag.  If \code{TRUE} each series is
    whitened using \code{\link{ar}}. }
  \item{ar.order.max}{ \code{NULL} or a positive integer giving the
    maximum order of the \code{\link{ar}} model used to prewhiten.
    Prewhitening removes as many years from the start of each series as
    the order of the model.  If \code{NULL}, the order is chosen by AIC
    up to the \code{\link{ar}} default, which on long series can be 20
    or more.  Requires \code{\var{prewhiten} = TRUE}. }
  \item{pcrit}{ a number between 0 and 1 giving the critical value for
    the correlation test. }
  \item{biweight}{ \code{logical} flag.  If \code{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}}. }
  \item{method}{Can be either \code{"pearson"}, \code{"kendall"}, or
    \code{"spearman"} which indicates the correlation coefficient to be
    used.  Defaults to \code{"spearman"}.  See \code{\link{cor.test}}. }
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
  \item{lag.max}{ a non-negative whole number less than
    \code{\var{seg.length}}.  Each segment is also correlated against the
    master shifted by every lag from \code{-\var{lag.max}} to
    \code{\var{lag.max}} years, and the best position is returned in
    \code{\var{best.lag}} and \code{\var{best.rho}}.  The default, 0,
    tests the dated position only, as before. }
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
  Spearman\enc{’}{'}s rho calculated via \code{\link{cor.test}} using
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

  As an alternative to the \code{\link{hanning}} filter, \code{\var{nyrs}}
  divides each series by a smoothing spline, which removes no years from
  the ends of the series.  COFECHA uses a 32-year spline.  The spline
  alone does not usually save years at the start of a series, because
  the \code{\link{ar}} model is then fitted to a high-pass filtered series,
  and AIC tends to choose a high order for it.  Setting
  \code{\var{ar.order.max}} to a small value (e.g., 3) limits the years
  lost to prewhitening, and after a spline a low-order model loses little
  of the fit.

  The function is typically invoked to produce a plot where each segment
  for each series is colored by its correlation to the master
  chronology.  Green segments are those that do not overlap completely
  with the width of the bin.  Blue segments are those that correlate
  above the user-specified critical value.  Red segments are those that
  correlate below the user-specified critical value and might indicate a
  dating problem.

  A segment that correlates well where it is dated can still correlate
  better somewhere else.  With \code{\var{lag.max}} greater than 0, each
  segment is also correlated against the master at every shift from
  \code{-\var{lag.max}} to \code{\var{lag.max}} years.  Each year of the
  segment is paired with the master value \var{k} years away, so the
  correlation at lag \var{k} is that of \code{series[t]} with
  \code{master[t + k]}.  The sign follows \code{\link{ccf.series.rwl}}
  with \code{\var{series.x} = FALSE}: a negative lag means missing rings
  in the series, a positive lag means false (extra) rings.  The dated
  position wins ties.

  This gives the two kinds of flag COFECHA reports.  An \sQuote{A}
  segment correlates below the critical value, but its dated position
  is still the best one tested: it is weak, not misdated.  A
  \sQuote{B} segment correlates better at some other position,
  whether or not it is significant as dated.  The letters are not
  stored; they follow from the returned matrices as
  \code{best.lag != 0} for B and
  \code{best.lag == 0 & p.val >= pcrit} for A (see Examples).  When
  \code{\var{lag.max}} is greater than 0, the plot draws B segments in
  purple and leaves red for the rest of the segments below the critical
  value.

  A B flag is a hypothesis about the dating, not a verdict.  A one-year
  shift that gains 0.05 in correlation is noise as often as it is a
  missing ring, which is why the size of the gain is returned
  (\code{best.rho - spearman.rho}) alongside the lag, so that it can be
  thresholded.  Check any B segment against the wood, for example with
  \code{\link{ccf.series.rwl}} and \code{\link{xskel.ccf.plot}}.

  Read the lags along a series rather than one bin at a time.  Dating
  runs from the bark inward, so a missing ring moves every ring before
  it one year late: every complete bin before the missing ring has
  \code{best.lag} of -1 and every bin after it has 0.  A false ring
  does the same with +1.  The error therefore lies where the lag
  changes, not in the bin with the largest gain.  The bin that
  straddles it holds some shifted and some unshifted years, and may
  show either lag with a small gain.  A run of -1 with correctly dated
  bins on both sides is a different fault: the ring count is right, but
  a ring is missing at the later end of the run and there is an extra
  ring at the earlier end.  A locally absent ring entered as a zero in
  too early a year looks exactly like this.  A test on the whole series,
  such as \code{RWL_DATING_LAG} in \code{\link{rwl.check}}, usually cannot
  see it, because most of the series is dated correctly.  The Examples
  show both patterns.

  The shifted correlations follow the same complete-overlap rule as the
  dated one: a lag is tested only if the master has a value for every
  year of the shifted window.  Near the start and end of the record
  (and of the master), the lags that run off the record cannot be
  tested, and a segment that lies within \code{\var{lag.max}} years of
  an end is only searched in one direction.  A segment that is not
  complete at its dated position is not tested at all.  Floating series
  and series with dating errors near their ends sit exactly where the
  test is blind, so the absence of a flag there says little.  Worse, a
  segment misdated in the direction that runs off the record can still
  be flagged B, at whichever of the remaining lags happens to correlate
  best, so the lag reported for a B near either end can be noise even
  when the segment really is misdated.
}
\value{
  A \code{list} containing matrices \code{\var{spearman.rho}},
  \code{\var{p.val}}, \code{\var{overall}}, \code{\var{bins}},  
  \code{\var{rwi}}, vector \code{\var{avg.seg.rho}},
  numeric \code{\var{seg.lag}}, \code{\var{seg.length}}, \code{\var{pcrit}}, 
  \code{\var{label.cex}}, matrices \code{\var{best.lag}},
  \code{\var{best.rho}} and numeric \code{\var{lag.max}}.  An additional \code{character}
  \code{\var{flags}} is also returned if any segments fall below the
  critical value.  Matrix \code{\var{spearman.rho}} contains the
  correlations for each series by bin.  Matrix \code{\var{p.val}}
  contains the p-values on the correlation for each series by
  bin.  Matrix \code{\var{overall}} contains the average correlation and
  p-value for each series.  Matrix \code{\var{bins}} contains the years
  encapsulated by each bin.  The vector \code{\var{avg.seg.rho}}
  contains the average correlation for each bin. Matrix \code{\var{rwi}}
  contains the detrended rwl data, the numerics \code{\var{seg.lag}}, 
  \code{\var{seg.length}}, \code{\var{pcrit}}, \code{\var{label.cex}} 
  are from the oroginal call and used to pass into \code{\var{plot.crs}}. 

  Matrices \code{\var{best.lag}} and \code{\var{best.rho}} have the same
  shape and names as \code{\var{spearman.rho}}.  \code{\var{best.lag}}
  gives, for each series and bin, the lag (in years) at which the
  segment correlates best with the master, and \code{\var{best.rho}}
  gives that correlation.  Both are \code{NA} where
  \code{\var{spearman.rho}} is.  With \code{\var{lag.max} = 0},
  \code{\var{best.lag}} is 0 and \code{\var{best.rho}} equals
  \code{\var{spearman.rho}}.  \code{\var{lag.max}} is from the original
  call.  Flags in \code{\var{flags}} depend on the p-value only and are
  the same whatever \code{\var{lag.max}} is.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{
  \code{\link{corr.series.seg}}, \code{\link{skel.plot}},
  \code{\link{series.rwl.plot}}, \code{\link{ccf.series.rwl}},
  \code{\link{plot.crs}}
}
\examples{library(utils)
data(co021)
crs <- corr.rwl.seg(co021, seg.length = 100, label.cex = 1.25)
names(crs)
## Average correlation and p-value for the first few series
head(crs$overall)
## Average correlation for each bin
crs$avg.seg.rho

## Plant a missing ring and follow it through the bins: delete the
## 1500 ring of series 641143, the same fault as in the examples for
## ccf.series.rwl(), corr.series.seg() and xskel.ccf.plot().  Dated
## from the bark, every ring before 1500 now sits one year late.
dat <- co021
x <- dat$"641143"
names(x) <- rownames(dat)
dat$"641143" <- delete.ring(x, year = 1500)
crs <- corr.rwl.seg(dat, lag.max = 5)
## -1 in every complete bin before 1500 (drawn in purple), 0 from 1500
## on, and a large gain in each shifted bin
ok <- !is.na(crs$best.lag["641143", ])
rbind(lag = crs$best.lag["641143", ok],
      rho = round(crs$spearman.rho["641143", ok], 2),
      best.rho = round(crs$best.rho["641143", ok], 2))

## The same missing ring with a false ring inserted at 1400.  The ring
## count is right again, and only the rings between the two errors are
## one year late.
dat$"641143" <- insert.ring(delete.ring(x, year = 1500), year = 1400)
crs <- corr.rwl.seg(dat, lag.max = 5, make.plot = FALSE)
## -1 between the two errors only, 0 on both sides
crs$best.lag["641143", ok]

## COFECHA-style A and B flags for every series
flag <- ifelse(crs$best.lag != 0, "B",
               ifelse(crs$p.val >= crs$pcrit, "A", ""))
flag[is.na(flag)] <- ""
## B segments with their lag and the gain in correlation
idx <- which(flag == "B", arr.ind = TRUE)
data.frame(series = rownames(flag)[idx[, 1]],
           bin = colnames(flag)[idx[, 2]],
           lag = crs$best.lag[idx],
           rho = round(crs$spearman.rho[idx], 3),
           best.rho = round(crs$best.rho[idx], 3))
}
\keyword{ manip }

