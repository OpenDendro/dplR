\encoding{UTF-8}
\name{sens1}
\alias{sens1}
\title{ Calculate Mean Sensitivity  }
\description{
  This function calculates mean sensitivity of a detrended ring-width
  series.
}
\usage{
  sens1(x)
}
\arguments{
  \item{x}{ a \code{numeric} vector containing the series }
}
\details{
  This calculates mean sensitivity according to Eq. 1 in Biondi and
  Qeadan (2008).  This is the standard measure of sensitivity in
  dendrochronology and is typically calculated on detrended series.
  However, note that mean sensitivity is not a robust statistic and 
  should rarely, if ever, be used (Bunn et al. 2013).

}
\value{ the mean sensitivity.
}
\references{

  Biondi, F. and Qeadan, F. (2008) Inequality in Paleorecords.
  \emph{Ecology}, \bold{89}(4), 1056\enc{–}{--}1067.

  Bunn, A. G., Jansma, E., Korpela, M., Westfall, R. D., and Baldwin,
  J. (2013) Using simulations and data to evaluate mean sensitivity
  (\eqn{\zeta}{zeta}) as a useful statistic in dendrochronology.
  \emph{Dendrochronologia}, \bold{31}(3), 250\enc{–}{--}254.
  
}
\author{ Mikko Korpela, based on original by Andy Bunn }
\seealso{ \code{\link{sens2}}, \code{\link{rwl.stats}} }
\examples{library(utils)
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
sens1(ca533.rwi[, 1])
}
\keyword{ univar }
