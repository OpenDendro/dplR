\name{rwi.stats.running}
\alias{rwi.stats.running}
\alias{rwi.stats}
\alias{rwi.stats.legacy}
\title{ (Running Window) Statistics on Detrended Ring-Width Series }
\description{
  This function calculates descriptive statistics on a \code{data.frame}
  of (usually) ring-width indices. The statistics are optionally
  computed in a running window with adjustable length and overlap.
}
\usage{
rwi.stats.running(rwi, ids = NULL, period = c("max", "common"),
                  running.window = TRUE,
                  window.length = min(50, nrow(rwi)),
                  window.overlap = floor(window.length / 2),
                  first.start = NULL,
                  min.corr.overlap = min(30, window.length),
                  round.decimals = 3,
                  zero.is.missing = TRUE)

rwi.stats(rwi, ids=NULL, period=c("max", "common"), \dots)

rwi.stats.legacy(rwi, ids=NULL, period=c("max", "common"))
}
\arguments{

  \item{rwi}{ a \code{data.frame} with detrended and standardized ring
    width indices as columns and years as rows such as that produced by
    \code{\link{detrend}}. }

  \item{ids}{ an optional \code{data.frame} with column one named
    \code{"tree"} giving a \code{numeric} \acronym{ID} for each tree and
    column two named \code{"core"} giving a \code{numeric} \acronym{ID}
    for each core. Defaults to one core per tree as
    \code{data.frame(tree = 1:ncol(\var{rwi}), core = rep(1,
      ncol(\var{rwi})))}. }

  \item{period}{ a \code{character} string, either \code{"common"} or
    \code{"max"} indicating whether correlations should be limited to
    complete observations over the period common to all cores (i.e. rows
    common to all samples) or the maximum pairwise overlap. Defaults
    to \code{"max"}. }

  \item{running.window}{ \code{logical} flag indicating whether to use a
    running window (\code{TRUE}, the default) or to ignore the other
    window parameters and effectively use one window covering all years
    (\code{FALSE}). }

  \item{window.length}{ \code{numeric} value indicating the length of
    the running window in years. The default is 50 years or the number
    of years (rows) in \code{\var{rwi}}, whichever is smaller. }

  \item{window.overlap}{ \code{numeric} value indicating the overlap of
    consecutive window positions, i.e. the number of common years. The
    default is half of the window length, rounded down. }

  \item{first.start}{ an optional \code{numeric} value setting the
    position of the first window. Must be a value between \code{1} and
    \code{\var{n.years}-\var{window.length}+1}, where
    \code{\var{n.years}} is the number of years in \code{\var{rwi}}. The
    default value \code{NULL} lets the function make the decision using
    some heuristic rules. }

  \item{min.corr.overlap}{ \code{numeric} value setting the minimum
    number of common years in any pair of ring-width series required for
    their correlation to be included in the calculations. Smaller
    overlaps are considered to yield unreliable correlation values which
    are ignored. Defaults to the minimum of 30 and the length of the
    window. One way to lift the restriction and include all correlations
    is to set \code{\var{min.corr.overlap} = 0}. }

  \item{round.decimals}{ non-negative integer \code{numeric} value
    setting the desired number of decimal places in the results. Use
    \code{NA}, \code{NULL} or a negative number for no rounding. }

  \item{zero.is.missing}{ \code{logical} flag indicating whether to
    treat zeros as missing values (\code{TRUE}, the default) or to
    include them in computation (\code{FALSE}).  }
  
  \item{\dots}{ arguments passed on to \code{rwi.stats.running} }

}
\details{
  This calculates a variety of descriptive statistics commonly used in
  dendrochronology.

  The function \code{rwi.stats} is a wrapper that calls
  \code{rwi.stats.running} with \code{\var{running.window} = FALSE}.
  The results may differ from those prior to dplR 1.5.3, where the
  former \code{rwi.stats} (now renamed to \code{rwi.stats.legacy}) was
  replaced with a call to \code{rwi.stats.running}.

  For correctly calculating the statistics on within and between series
  variability, an appropriate mask (parameter \code{\var{ids}}) must be
  provided that identifies each series with a tree as it is common for
  dendrochronologists to take more than one core per tree.  The function
  \code{\link{read.ids}} is helpful for creating a mask based on the
  series \acronym{ID}.

  If \code{\var{ids}} has duplicate tree/core combinations, the
  corresponding series are averaged before any statistics are computed.
  The value of the parameter \code{\var{zero.is.missing}} is relevant in
  the averaging: \code{TRUE} ensures that zeros don't contribute to the
  average.  The default value of \code{\var{zero.is.missing}} is
  \code{TRUE}.  The default prior to dplR 1.5.3 was \code{FALSE}. If the
  parameter is set to \code{FALSE}, the user will be warned in case
  zeros are present.  Duplicate tree/core combinations are not detected
  by \code{rwi.stats.legacy}.

  Row names of \code{\var{ids}} may be used for matching the
  \acronym{ID}s with series in \code{\var{rwi}}.  In this case, the
  number of rows in \code{\var{ids}} is allowed to exceed the number of
  series.  If some names of \code{\var{rwi}} are missing from the row
  names of \code{\var{ids}}, the rows of \code{\var{ids}} are assumed to
  be in the same order as the columns of \code{\var{rwi}}, and the
  dimensions must match.  The latter is also the way that
  \code{rwi.stats.legacy} handles \code{\var{ids}}, i.e. names are
  ignored and dimensions must match.

  Note that \code{period = "common"} can produce \code{NaN} for many of
  the stats if there is no common overlap period among the cores.  This
  happens especially in chronologies with floating subfossil samples
  (e.g., \code{\link{ca533}}).

  Some of the statistics are specific to dendrochronology (e.g., the
  effective number of cores or the expressed population signal).  Users
  unfamiliar with these should see Cook and Kairiukstis (1990) and
  Fritts (2001) for further details.

}
\value{ A \code{data.frame} containing the following columns (each row
  corresponds to one position of the window):

  \item{start.year}{the first year in the window.  Not returned if
    \code{running.window} is \code{FALSE} or called as
    \code{rwi.stats} or \code{rwi.stats.legacy}.}

  \item{mid.year}{the middle year in the window, rounded down.  Not
    returned if \code{running.window} is \code{FALSE} or called as
    \code{rwi.stats} or \code{rwi.stats.legacy}.}
  
  \item{end.year}{the last year in the window.  Not returned if
    \code{running.window} is \code{FALSE} or called as
    \code{rwi.stats} or \code{rwi.stats.legacy}.}

  \item{n.tot}{total number of correlations computed}

  \item{n.wt}{number of within-tree correlations computed}

  \item{n.bt}{number of between-tree correlations computed}

  \item{rbar.tot}{the mean of all correlation between different cores}

  \item{rbar.wt}{the mean of the correlations between series from the
    same tree over all trees}

  \item{rbar.bt}{the mean interseries correlation between all series
    from different trees}

  \item{c.eff}{the effective number of cores (takes into account the
    number of within-tree correlations in each tree)}

  \item{rbar.eff}{the effective signal calculated as \code{
      \var{rbar.bt} / (\var{rbar.wt} + (1-\var{rbar.wt}) / \var{c.eff}) }}

  \item{eps}{the expressed population signal}

  \item{n}{the average number of trees (for each year, a tree needs at
    least one non-\code{NA} core in order to be counted).  Not returned
    in the results of \code{rwi.stats.legacy}.}
  
}
\references{

  Cook, E. R. and Kairiukstis, L. A. (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
  Springer.  \acronym{ISBN-13}: 978-0792305866.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.  Blackburn.
  \acronym{ISBN-13}: 978-1930665392.
  
}
\note{
  This function uses the \code{\link[foreach]{foreach}} looping
  construct with the \code{\link[foreach:foreach]{\%dopar\%}} operator.
  For parallel computing and a potential speedup, a parallel backend
  must be registered before running the function.
}
\author{ Mikko Korpela, based on \code{\link{rwi.stats.legacy}} by Andy
  Bunn }
\seealso{ \code{\link{detrend}}, \code{\link{cor}},
  \code{\link{read.ids}}, \code{\link{rwi.stats}} }
\examples{data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "Mean") # mean is fast
ca533.ids <- read.ids(ca533, stc = c(3, 2, 3))
rwi.stats.running(ca533.rwi, ca533.ids)
rwi.stats(ca533.rwi, ca533.ids)        # i.e. running.window = FALSE
rwi.stats.legacy(ca533.rwi, ca533.ids) # rwi.stats prior to dplR 1.5.3
\dontrun{
## Plot the chronology showing a potential cutoff year based on eps
ca533.rwi <- detrend(rwl = ca533, method = "Spline") # detrend again
ca533.crn <- chron(ca533.rwi)
## Running stats on the rwi with an 80 yr window
foo <- rwi.stats.running(ca533.rwi, ca533.ids, window.length = 80)
yrs <- as.numeric(rownames(ca533.crn))
bar <- data.frame(yrs = c(min(yrs), foo$mid.year, max(yrs)),
                  eps = c(NA, foo$eps, NA))
par(mar = c(3, 3, 3, 3), mgp = c(1.25, 0.25, 0), tcl = 0.25,
    mfcol = c(2, 1))
plot(yrs, ca533.crn[, 1], type = "n", xlab = "Years", ylab = "RWI")
xx <- c(500, 500, max(bar$yrs[bar$eps < 0.8], na.rm = TRUE),
        max(bar$yrs[bar$eps < 0.8], na.rm = TRUE))
yy <- c(-1, 3, 3, -1)
polygon(xx, yy, col = "grey80")
abline(h = 1, lwd = 1.5)
lines(yrs, ca533.crn[, 1], col = "grey50")
lines(yrs, ffcsaps(ca533.crn[, 1], nyrs = 32), col = "red", lwd = 2)
par(new = TRUE)

## Second plot is the chronology after the cut off only
plot(bar$yrs, bar$eps, type = "b", xlab = "", ylab = "", axes = FALSE,
     pch = 20, col = "blue")
axis(4, at = pretty(foo$eps))
mtext("EPS", side = 4, line = 1.25)
box()
yr.mask <- yrs > max(bar$yrs[bar$eps<0.8], na.rm = TRUE)
plot(yrs[yr.mask], ca533.crn[yr.mask, 1], type = "n",
     xlab = "Years", ylab = "RWI")
abline(h = 1, lwd = 1.5)
lines(yrs[yr.mask], ca533.crn[yr.mask, 1], col = "grey50")
lines(yrs[yr.mask], ffcsaps(ca533.crn[yr.mask, 1], nyrs = 32),
      col = "red", lwd = 2)
}
}
\keyword{ misc }
