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
  that is typically used in the presence of a trend.  However, note 
  that that mean sensitivity is not a robust statitic and should rarely, 
  if ever, be used (Bunn et al. 2013).
}
\value{ the mean sensitivity.
}
\references{

  Biondi, F. and Qeadan, F. (2008) Inequality in Paleorecords.
  \emph{Ecology}, 89(4):1056\enc{–}{--}1067.

  Bunn. A.G., Jansma E., Korpela M., Westfall R.D., and Baldwin J. (2013) 
  Using simulations and data to evaluate mean sensitivity (zeta) as a useful 
  statistic in dendrochronology Dendrochronologia 31 250–4.
  
}
\author{ Mikko Korpela, based on original by Andy Bunn }
\seealso{ \code{\link{sens1}}, \code{\link{rwl.stats}} }
\examples{data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
sens2(ca533.rwi[, 1])
}
\keyword{ univar }
