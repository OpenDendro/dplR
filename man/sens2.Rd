\name{sens2}
\alias{sens2}
\title{ Calculate Mean Sensitivity on Series with a Trend }
\description{
  This function calculates mean sensitivity of a raw or detrended
  ring-width series.
}
\usage{
  sens2(x)
}
\arguments{
  \item{x}{ a \code{numeric} vector containing the series }
}
\details{
  This calculates mean sensitivity according to Eq. 2 in Biondi and
  Qeadan (2008).  This is a measure of sensitivity in dendrochronology
  that is typically used in the presence of a trend.  Users unfamiliar
  with sensitivity in tree-ring data should see Cook and Kairiukstis
  (1990) and Fritts (2001) for further details.
}
\value{ the mean sensitivity.
}
\references{

  Biondi, F. and Qeadan, F. (2008) Inequality in Paleorecords.
  \emph{Ecology}, 89(4):1056\enc{–}{--}1067.

  Cook, E. R. and Kairiukstis, L. A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences}.
  Springer.  \acronym{ISBN-13}: 978-0792305866.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.  Blackburn.
  \acronym{ISBN-13}: 978-1930665392.
  
}
\author{ Mikko Korpela, based on original by Andy Bunn }
\seealso{ \code{\link{sens1}}, \code{\link{rwl.stats}} }
\examples{data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
sens2(ca533.rwi[, 1])
}
\keyword{ univar }
