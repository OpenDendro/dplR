\encoding{UTF-8}
\name{rwi.stats.running}
\alias{rwi.stats.running}
\alias{rwi.stats}
\alias{rwi.stats.legacy}
\title{ (Running Window) Statistics on Detrended Ring-Width Series }
\description{
  These functions calculate descriptive statistics on a
  \code{data.frame} of (usually) ring-width indices.  The statistics are
  optionally computed in a running window with adjustable length and
  overlap.  The data can be filtered so that the comparisons are made to
  on just high-frequency data.
}
\usage{
rwi.stats.running(rwi, ids = NULL, period = c("max", "common"),
                  method = c("spearman", "pearson","kendall"),
                  prewhiten=FALSE,n=NULL,
                  running.window = TRUE,
                  window.length = min(50, nrow(rwi)),
                  window.overlap = floor(window.length / 2),
                  first.start = NULL,
                  min.corr.overlap = min(30, window.length),
                  round.decimals = 3,
                  zero.is.missing = TRUE)

rwi.stats(rwi, ids=NULL, period=c("max", "common"), 
          method = c("spearman", "pearson","kendall"), \dots)

rwi.stats.legacy(rwi, ids=NULL, period=c("max", "common"))
}
\arguments{

  \item{rwi}{ a \code{data.frame} with detrended and standardized ring
    width indices as columns and years as rows such as that produced by
    \code{\link{detrend}}. }

  \item{ids}{ an optional \code{data.frame} with column one named
    \code{"tree"} giving a \code{numeric} \acronym{ID} for each tree and
    column two named \code{"core"} giving a \code{numeric} \acronym{ID}
    for each core.  Defaults to one core per tree as\cr
    \code{data.frame(tree=1:ncol(\var{rwi}), core=rep(1, ncol(\var{rwi})))}. }
  \item{period}{ a \code{character} string, either \code{"common"} or
    \code{"max"} indicating whether correlations should be limited to
    complete observations over the period common to all cores (i.e. rows
    common to all samples) or the maximum pairwise overlap.  Defaults
    to \code{"max"}. }
  \item{method}{Can be either \code{"pearson"}, \code{"kendall"}, or
    \code{"spearman"} which indicates the correlation coefficient to be
    used.  Defaults to \code{"spearman"}.  See \code{\link{cor}}. }
  \item{n}{ \code{NULL} or an integral value giving the filter length
    for the \code{\link{hanning}} filter used for removal of low
    frequency variation. }
  \item{prewhiten}{ \code{logical} flag.  If \code{TRUE} each series is
    whitened using \code{\link{ar}}. }
  \item{running.window}{ \code{logical} flag indicating whether to use a
    running window (\code{TRUE}, the default) or to ignore the other
    window parameters and effectively use one window covering all years
    (\code{FALSE}). }
  \item{window.length}{ \code{numeric} value indicating the length of
    the running window in years.  The default is 50 years or the number
    of years (rows) in \code{\var{rwi}}, whichever is smaller. }
  \item{window.overlap}{ \code{numeric} value indicating the overlap of
    consecutive window positions, i.e. the number of common years.  The
    default is half of the window length, rounded down. }
  \item{first.start}{ an optional \code{numeric} value setting the
    position of the first window.  Must be a value between \code{1} and
    \code{\var{n.years}-\var{window.length}+1}, where
    \code{\var{n.years}} is the number of years in \code{\var{rwi}}.  The
    default value \code{NULL} lets the function make the decision using
    some heuristic rules. }
  \item{min.corr.overlap}{ \code{numeric} value setting the minimum
    number of common years in any pair of ring-width series required for
    their correlation to be included in the calculations.  Smaller
    overlaps are considered to yield unreliable correlation values which
    are ignored.  Defaults to the minimum of 30 and the length of the
    window.  One way to lift the restriction and include all correlations
    is to set \code{\var{min.corr.overlap} = 0}. }
  \item{round.decimals}{ non-negative integer \code{numeric} value
    setting the desired number of decimal places in the results.  Use
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
  the averaging: \code{TRUE} ensures that zeros don\enc{’}{'}t
  contribute to the average.  The default value of
  \code{\var{zero.is.missing}} is \code{TRUE}.  The default prior to
  dplR 1.5.3 was \code{FALSE}.  If the parameter is set to \code{FALSE},
  the user will be warned in case zeros are present.  Duplicate
  tree/core combinations are not detected by \code{rwi.stats.legacy}.

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
  Fritts (2001) for further details for computational details on the
  output.  The signal-to-noise ratio is calculated following Cook and 
  Pederson (2011).
  
  Note that Buras (2017) cautions against using the expressed population signal as a statistic to determine the whether a chronology correctly represents the population signal of a data set. He reccomends the use of subsample signal strength (\code{\link{sss}}) over EPS.

  If desired, the \code{\var{rwi}} can be filtered in the same manner
  as the family of cross-dating functions using \code{\var{prewhiten}} and
  \code{\var{n}}.  See the help page for \code{\link{corr.rwl.seg}} for
  more details.
  

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

  \item{n.cores}{the number of cores}

  \item{n.trees}{the number of trees}
  
  \item{n}{the average number of trees (for each year, a tree needs at
    least one non-\code{NA} core in order to be counted).  Not returned
    in the results of \code{rwi.stats.legacy}}
  
  \item{n.tot}{total number of correlations calculated as
    \code{\var{n.wt} + \var{n.bt}}.

    Equal to \code{\var{n.cores} * (\var{n.cores}-1)/2} if there is
    overlap between all samples }

  \item{n.wt}{number of within-tree correlations computed}

  \item{n.bt}{number of between-tree correlations computed}

  \item{rbar.tot}{the mean of all the correlations between different 
      cores}

  \item{rbar.wt}{the mean of the correlations between series from the
    same tree over all trees}

  \item{rbar.bt}{the mean interseries correlation between all series
    from different trees}

  \item{c.eff}{the effective number of cores (takes into account the
    number of within-tree correlations in each tree)}

  \item{rbar.eff}{the effective signal calculated as
    \code{\var{rbar.bt} / (\var{rbar.wt} + (1-\var{rbar.wt}) / \var{c.eff}) }}

  \item{eps}{the expressed population signal calculated using the average
    number of trees as\cr
    \code{\var{n} * \var{rbar.eff} / ((\var{n} - 1) * \var{rbar.eff} + 1)} }

  \item{snr}{the signal to noise ratio calculated using the average
    number of trees as\cr
    \code{\var{n} * \var{rbar.eff} / (1-\var{rbar.eff})} }
}
\references{

  Buras, A. (2017) A comment on the Expressed Population Signal. Dendrochronologia 44:130-132. 

  Cook, E. R. and Kairiukstis, L. A., editors (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
    Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Cook, E. R. and Pederson, N. (2011) Uncertainty, Emergence, and
  Statistics in Dendrochronology.  In Hughes, M. K., Swetnam, T. W., and
  Diaz, H. F., editors, \emph{Dendroclimatology: Progress and
  Prospects}, pages 77\enc{–}{--}112.  Springer.  \acronym{ISBN-13}:
  978-1-4020-4010-8.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.  Blackburn.
    \acronym{ISBN-13}: 978-1-930665-39-2.
  
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
  \code{\link{read.ids}}, \code{\link{rwi.stats}},
  \code{\link{corr.rwl.seg}}  } 
\examples{library(utils)
data(gp.rwl)
data(gp.po)
gp.rwi <- cms(rwl = gp.rwl, po = gp.po)
gp.ids <- read.ids(gp.rwl, stc = c(0, 2, 1))
# On a running window
rwi.stats.running(gp.rwi, gp.ids)
## With no running window (i.e. running.window = FALSE)
rwi.stats(gp.rwi, gp.ids)
## Restrict to common overlap (in this case 1899 to 1987)
rwi.stats(gp.rwi, gp.ids, period="common")
rwi.stats.legacy(gp.rwi, gp.ids) # rwi.stats prior to dplR 1.5.3

}
\keyword{ misc }
