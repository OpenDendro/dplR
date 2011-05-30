\name{rwi.stats.running}
\alias{rwi.stats.running}
\title{ Calculates Running Window Statistics on Detrended Ring Width Series }
\description{
  This function calculates descriptive statistics on a \code{data.frame} of
  (usually) ring-width indices. The statistics are computed in a running
  window with adjustable length and overlap.
}
\usage{
rwi.stats.running(rwi, ids=NULL, period=c("max", "common"),
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
\note{
  This function uses the \code{\link[foreach]{foreach}} looping
  construct with the \code{\link[foreach]{\%dopar\%}} operator.  For
  parallel computing and a potential speedup, a parallel backend must be
  registered before running the function.
}
\author{ Mikko Korpela, based on \code{\link{rwi.stats}} by Andy Bunn }
\seealso{ \code{\link{detrend}}, \code{\link[stats]{cor}},
  \code{\link{read.ids}}, \code{\link{rwi.stats}} }
\examples{
  data(ca533)
  ca533.rwi <- detrend(rwl = ca533, method = "Spline")
  ca533.ids <- read.ids(ca533, stc=c(3,2,3))
  rwi.stats.running(ca533.rwi, ca533.ids)

  # plot the chronology showing a potential cutoff year based on eps
  ca533.crn <- chron(ca533.rwi)
  # running stats on the rwi with an 80-yr window
  foo <-rwi.stats.running(ca533.rwi, ca533.ids, window.length=80)
  yrs <- as.numeric(rownames(ca533.crn))
  yrs <- as.numeric(rownames(ca533.crn))
  bar <- data.frame(yrs=c(min(yrs),foo$mid.year,max(yrs)),eps=c(NA,foo$eps,NA))
  par(mar = c(3, 3, 3, 3), mgp = c(1.25, 0.25, 0), tcl = 0.25, mfcol=c(2,1))
  plot(yrs,ca533.crn[,1],type='n', xlab = "Years", ylab = "RWI")
  xx=c(500,500,max(bar$yrs[bar$eps < 0.8],na.rm=TRUE),max(bar$yrs[bar$eps < 0.8],na.rm=TRUE))
  yy=c(-1,3,3,-1)
  polygon(xx,yy,col='grey80')
  abline(h=1,lwd=1.5)
  lines(yrs,ca533.crn[,1],col='grey50')
  lines(yrs,ffcsaps(ca533.crn[,1],nyrs=32),col='red',lwd=2)
  par(new = TRUE)
  # second plot is the chronology after the cut off only
  plot(bar$yrs,bar$eps, ,type='b',xlab = "", ylab = "", axes = FALSE,
       pch=20,col='blue')
  axis(4, at = pretty(foo$eps))
  mtext("EPS", side = 4, line = 1.25)
  box()
  yr.mask <- yrs>max(bar$yrs[bar$eps<0.8],na.rm=TRUE)
  plot(yrs[yr.mask],ca533.crn[yr.mask,1],type='n', xlab = "Years", ylab = "RWI")
  abline(h=1,lwd=1.5)
  lines(yrs[yr.mask],ca533.crn[yr.mask,1],col='grey50')
  lines(yrs[yr.mask],ffcsaps(ca533.crn[yr.mask,1],nyrs=32),col='red',lwd=2)

}
\keyword{ misc }
