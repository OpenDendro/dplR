\name{rwi.stats.running}
\alias{rwi.stats.running}
\title{ Calculates Running Window Statistics on Detrended Ring Width Series }
\description{
  This function calculates descriptive statistics on a \code{data.frame} of
  (usually) ring-width indices. The statistics are computed in a running
  window with adjustable length and overlap.
}
\usage{
rwi.stats.running(rwi, ids=NULL, period="max",
                  running.window=TRUE,
                  window.length=min(50,nrow(rwi)),
                  window.overlap=floor(window.length/2),
                  first.start=NULL,
                  min.corr.overlap=min(30,window.length),
                  round.decimals=3)
}
\arguments{
  \item{rwi}{ a \code{data.frame} with detrended and standardised ring
  width indices as columns and years as rows such as that produced by
  \code{\link{detrend}}. }
  \item{ids}{ an optional \code{data.frame} with column one named
  \dQuote{tree} giving a numeric id for each tree and column two named
  \dQuote{core} giving a numeric id for each core. Defaults to one core
  per tree as
  \code{data.frame(tree=1:ncol(rwi),core=rep(1,ncol(rwi)))}. }
  \item{period}{ a character string, either \dQuote{common} or
  \dQuote{max} indicating whether correlations should be limited to
  complete observations over the period common to all cores (i.e. rows
  common to all samples) or the maximum pairwise overlap. Defaults
  to \dQuote{max}. }
  \item{running.window}{ logical flag indicating whether to use a
  running window (\code{TRUE}, the default) or to ignore the other
  window parameters and effectively use one window covering all years
  (\code{FALSE}). }
  \item{window.length}{ numeric value indicating the length of the
  running window in years. The default is 50 years or the number of
  years (rows) in \code{rwi}, whichever is smaller. }
  \item{window.overlap}{ numeric value indicating the overlap of
  consecutive window positions, i.e. the number of common years. The
  default is half of the window length, rounded down. }
  \item{first.start}{ an optional numeric value setting the position of
  the first window. Must be a value between \code{1} and
  \code{n.years-window.length+1}, where \code{n.years} is the number of
  years in \code{rwi}. The default value \code{NULL} lets the function
  make the decision using some heuristic rules. }
  \item{min.corr.overlap}{ numeric value setting the minimum number of
  common years in any pair of ring width series required for their
  correlation to be included in the calculations. Smaller overlaps are
  considered to yield unreliable correlation values which are
  ignored. Defaults to the minimum of 30 and the length of the
  window. One way to lift the restriction and include all correlations
  is to set \code{min.corr.overlap=0}. }
  \item{round.decimals}{ the desired number of decimal places in the
  results. }
}
\details{
  This calculates a variety of descriptive statistics commonly used in
  dendrochronology.

  For correctly calculating the statistics on within and between series
  variability, an appropriate mask must be provided that identifies each
  series with a tree as it is common for dendrochronologists to take more
  than one core per tree. The function \code{\link{read.ids}} is be helpful for
  creating a mask based on the series id.

  Note that period=\dQuote{common} can produce \code{NaN} for many of the
  stats if there is no common overlap period among the cores. This
  happens especially in chronologies with floating subfossil samples
  (e.g., \code{\link{ca533}}).

  Some of the statistics are specific to dendrochronology (e.g., the
  effective number of cores or the expressed population signal). Users
  unfamiliar with these should see Cook and Kairiukstis (1990) and Fritts
  (2001) for further details.

}
\value{ A \code{data.frame} containing the following columns (each row
  corresponds to one position of the window):
\code{start.year} the first year in the window (not present
if \code{running.window} is \code{FALSE}),
\code{mid.year} the middle year in the window, rounded down (not present
if \code{running.window} is \code{FALSE}),
\code{end.year} the last year in the window (not present
if \code{running.window} is \code{FALSE}),
\code{n.tot} total number of correlations computed,
\code{n.wt} number of within-tree correlations computed,
\code{n.bt} number of between-tree correlations computed,
\code{rbar.tot} the mean of all correlation between different cores,
\code{rbar.wt} the mean of the correlations between series from the same
tree over all trees,
\code{rbar.bt} the mean interseries correlation between all series from
different trees,
\code{c.eff} the effective number of cores (takes into account the
number of within-tree correlations in each tree),
\code{rbar.eff} the effective signal calculated as \code{ rbar.bt /
  (rbar.wt + (1-rbar.wt) / c.eff) },
\code{eps} the expressed population signal,
\code{n} the average number of trees (for each year, a tree needs
at least one non-NA core in order to be counted).
}
\references{ Cook, E.R. and Kairiukstis, L.A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences.} Springer.
  ISBN-13: 978-0792305866.

  Fritts, H.C. (2001) \emph{Tree Rings and Climate.} Blackburn. ISBN-13:
  978-1930665392.

}
\author{ Mikko Korpela, based on \code{\link{rwi.stats}} by Andy Bunn }
\seealso{ \code{\link{detrend}}, \code{\link[stats]{cor}},
  \code{\link{read.ids}}, \code{\link{rwi.stats}} }
\examples{

  data(ca533)
  ca533.rwi <- detrend(rwl = ca533, method = "Spline")
  ca533.crn <- chron(ca533.rwi)
  yrs <- as.numeric(rownames(ca533.crn))
  ca533.ids <- read.ids(ca533, stc=c(3,2,3))
  # running stats on the rwi with 100 yr window
  rwi.stats.running(ca533.rwi, ca533.ids, window.length=100)
}
\keyword{ misc }
