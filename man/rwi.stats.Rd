\name{rwi.stats}
\alias{rwi.stats}
\title{ Calculate Summary Statistics on Detrended Ring-Width Series }
\description{
  This function calculates descriptive statistics on a \code{data.frame}
  of (usually) ring-width indices.
}
\usage{
rwi.stats(rwi, ids = NULL, period = c("max", "common"))
}
\arguments{

  \item{rwi}{ a \code{data.frame} with detrended and standardized
    ring-width indices as columns and years as rows such as that
    produced by \code{\link{detrend}}. }

  \item{ids}{ an optional \code{data.frame} with column one named
    \code{"tree"} giving a \code{numeric} \acronym{ID} for each tree and
    column two named \code{"core"} giving a \code{numeric} \acronym{ID}
    for each core. Defaults to one core per tree as
    \code{data.frame(tree = 1:ncol(\var{rwi}), core = rep(1,
    ncol(\var{rwi})))}. }

\item{period}{ a \code{character} string, either \code{"common"} or
    \code{"max"} indicating whether correlations should be limited to
    complete observations over the period common to all cores or the
    maximum pairwise overlap. See Details of \code{\link{cor}}. Defaults
    to \code{"max"}. }
  
}
\details{
  This calculates a variety of descriptive statistics commonly used in
  dendrochronology.

  For correctly calculating the statistics on within and between series
  variability, an appropriate mask must be provided that identifies each
  series with a tree as it is common for dendrochronologists to take
  more than one core per tree. The function \code{\link{read.ids}} is be
  helpful for creating a mask based on the series \acronym{ID}.

  Note that \code{\var{period} = "common"} can produce \code{NaN} for
  many of the stats if there is no common overlap period among the
  cores. This happens especially in chronologies with floating subfossil
  samples (e.g., \code{\link{ca533}}).

  Some of the statistics are specific to dendrochronology (e.g., the
  effective number of cores or the expressed population signal). Users
  unfamiliar with these should see Cook and Kairiukstis (1990) and Fritts
  (2001) for further details.
}
\value{ A \code{data.frame} containing

  \item{n.tot}{total number of unique combinations of the input series
    (i.e., \code{\var{n}*(\var{n}-1)/2}, where \var{n} is the number of
    series)}

  \item{n.wt}{total number of unique combinations of the within-tree
    series}

  \item{n.bt}{total number of unique combinations of the between-tree
    series}

  \item{rbar.tot}{the mean of all correlation between different cores}

  \item{rbar.wt}{the mean of the correlations between series from the
    same tree over all trees}

  \item{rbar.bt}{the mean interseries correlation between all series
    from different trees}

  \item{c.eff}{the effective number of cores}

  \item{rbar.eff}{the effective signal calculated as \code{
    \var{rbar.bt} / (\var{rbar.wt} + (1-\var{rbar.wt}) / \var{c.eff}) }}

  \item{eps}{the expressed population signal}
}
\references{

  Cook, E. R. and Kairiukstis, L. A. (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
  Springer.  \acronym{ISBN-13}: 978-0792305866.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.  Blackburn.
  \acronym{ISBN-13}: 978-1930665392.
  
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{detrend}}, \code{\link{cor}},
  \code{\link{read.ids}}, \code{\link{rwi.stats.running}} }
\examples{data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.ids <- read.ids(ca533, stc = c(3,2,3))
rwi.stats(ca533.rwi, ca533.ids)
}
\keyword{ misc }
