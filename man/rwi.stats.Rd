\name{rwi.stats}
\alias{rwi.stats}
\title{ Calculates Summary Statistics on Detrended Ring Width Series }
\description{
  This function calculates descriptive statistics on a \code{data.frame} of
  (usually) ring-width indices.
}
\usage{
  rwi.stats(rwi, ids = NULL, period = "max")
}
\arguments{
  \item{rwi}{ a \code{data.frame} with detreneded and standardised ring width
  indices as columns and years as rows such as that produced by
  \code{\link{detrend}}. }
  \item{ids}{ a optional \code{data.frame} with column one named \dQuote{tree}
  giving a numeric id for each tree and column two named \dQuote{core} giving a
  numeric id for each core. Defaults to one core per tree as
  \code{data.frame(tree=1:ncol(rwi),core=rep(1,ncol(rwi)))}. }
  \item{period}{ a character string, either \dQuote{common} or \dQuote{max}
  indicating whether correlations should be limited to complete
  observations over the period common to all cores or the maximum
  pairwise overlap. See Details of \code{\link[stats]{cor}}. Defaults to
  \dQuote{max}. }
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
\value{ A \code{data.frame} containing the following:
    \code{n.tot} total number of unique combinations of the input series (i.e.,
    n*n-1/2),
    \code{n.wt} total number of unique combinations of the within-tree
    series,
    \code{n.bt} total number of unique combinations of the between-tree
    series,
    \code{rbar.tot} the mean of all correlation between different cores,
    \code{rbar.wt} the mean of the correlations between series from the same
    tree over all trees,
    \code{rbar.bt} the mean interseries correlation between all series from
    different trees,
    \code{c.eff} the effective number of cores,
    \code{rbar.eff} the effective signal calculated as \code{ rbar.bt /
    (rbar.wt + (1-rbar.wt) / c.eff) },
    \code{eps} the expressed population signal.
}
\references{ Cook, E.R. and Kairiukstis, L.A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences.} Springer.
  ISBN-13: 978-0792305866.

  Fritts, H.C. (2001) \emph{Tree Rings and Climate.} Blackburn. ISBN-13:
  978-1930665392.

}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{detrend}}, \code{\link[stats]{cor}},
  \code{\link{read.ids}}, \code{\link{rwi.stats.running}} }
\examples{

  data(ca533)
  ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
  ca533.ids <- read.ids(ca533, stc=c(3,2,3))
  rwi.stats(ca533.rwi, ca533.ids)
}
\keyword{ misc }
