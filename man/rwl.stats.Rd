\name{rwl.stats}
\alias{rwl.stats}
\alias{summary.rwl}
\title{ Calculate Descriptive Summary Statistics on Ring-Width Series }
\description{
  This function calculates descriptive statistics on a \code{rwl} object
  of raw or detrended ring-width series.
}
\usage{
rwl.stats(rwl)

\method{summary}{rwl}(object, ...)

}
\arguments{

  \item{rwl, object}{ a \code{rwl} object with (usually) raw ring-width series
  as columns and years as rows such as that produced by
  \code{\link{read.rwl}}.  It is sometimes desirable to run this on
  detrended (e.g., rwi) data. }

  \item{\dots}{ Additional arguments from the generic function.  These
    are silently ignored. }

}

\details{

  This calculates a variety of descriptive statistics commonly used in
  dendrochronology (see below). Users unfamiliar with these should see
  Cook and Kairiukstis (1990) and Fritts (2001) for further details.

  The \code{\link{summary}} method for class \code{"rwl"} is a wrapper
  for \code{rwl.stats}.
  
}

\value{
  A \code{data.frame} containing descriptive stats on each
  \code{"series"}. These are the first and last year of the series as
  well as the length of the series (\code{"first"}, \code{"last"},
  \code{"year"}).  The mean, median, standard deviation are given
  (\code{"mean"}, \code{"median"}, \code{"stdev"}) as are the skewness,
  two measures of sensitivity, the Gini coefficient, and first order
  autocorrelation (\code{"skew"}, \code{"\link{sens1}"},
  \code{"\link{sens2}"}, \code{"\link{gini.coef}"}, \code{"ar1"}).
  
  Note that that mean sensitivity is not a robust statitic that should rarely,
  if ever, be used (Bunn et al. 2013).
}
\references{

  Bunn. A.G., Jansma E., Korpela M., Westfall R.D., and Baldwin J. (2013) 
  Using simulations and data to evaluate mean sensitivity (zeta) as a useful 
  statistic in dendrochronology Dendrochronologia 31 250–4.

  Cook, E. R. and Kairiukstis, L.A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences}.
  Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.  Blackburn.
  \acronym{ISBN-13}: 978-1-930665-39-2.

}
\author{ Andy Bunn. Slightly improved by Mikko Korpela. }
\seealso{ \code{\link{rwi.stats}}, \code{\link{read.rwl}} }
\examples{data(ca533)
rwl.stats(ca533)
summary(ca533)
}
\keyword{ misc }
