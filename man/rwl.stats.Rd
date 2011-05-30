\name{rwl.stats}
\alias{rwl.stats}
\title{ Calculates Descriptive Statistics on Ring Width Series }
\description{
  This function calculates descriptive statistics on a \code{data.frame} of
  raw or detrended ring-width series.
}
\usage{
  rwl.stats(rwl)
}
\arguments{
  \item{rwl}{ a \code{data.frame} with (usually) raw ring-width series as
  columns and years as rows such as that produced by \code{\link{read.rwl}}.
  It is sometimes desireable to run this on detrended (e.g., rwi)
  data particularly for the sensitivity measures ( \code{\link{sens1}},
  \code{\link{sens2}}, \code{\link{gini.coef}}). }
}
\details{
  This calculates a variety of descriptive statistics commonly used in
  dendrochronology (see below). Users unfamiliar with these should see Cook
  and Kairiukstis (1990) and Fritts (2001) for further details.

}
\value{ A \code{data.frame} containing descriptive stats on
  each \code{series}. These are the first and last year of the series
  as well as the length of the series (\code{first},\code{last},\code{year}).
  The mean, median, standard deviation are given (\code{mean},\code{median},
  \code{stdev}) as are the skewness, two measures of sensitivity, the Gini
  coefficent, and first order autocorrelation (\code{skew},\code{\link{sens1}},
  \code{\link{sens2}}, \code{\link{gini.coef}}, \code{ar1}).
}
\references{ Cook, E.R. and Kairiukstis, L.A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences.} Springer.
  ISBN-13: 978-0792305866.

  Fritts, H.C. (2001) \emph{Tree Rings and Climate.} Blackburn. ISBN-13:
  978-1930665392.

}
\author{ Andy Bunn }
\seealso{ \code{\link{rwi.stats}}, \code{\link{read.rwl}} }
\examples{

  data(ca533)
  rwl.stats(ca533)

}
\keyword{ misc }
