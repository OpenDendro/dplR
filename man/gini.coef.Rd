\name{gini.coef}
\alias{gini.coef}
\title{ Calculate the Gini Coefficient   }
\description{
  This function calculates the Gini coefficient on raw or detrended
  ring-width series.
}
\usage{
gini.coef(x)
}
\arguments{
  \item{x}{ a \code{numeric} vector }
}
\details{
  This calculates the Gini coefficient of inequality which is used as an
  all-lag measure of diversity in tree-ring records - typically detrended
  series. Lower values indicate lower diversity. The use of the Gini
  coefficient in dendrochronology is described by Biondi and Qeadan (2008).
  See Handcock and Morris (1999) for more information.
}
\value{ the Gini coefficient.
}
\references{
  Biondi, F. and Qeadan, F. (2008) Inequality in Paleorecords.
  \emph{Ecology},  89(4):1056--1067.

  Handcock. M. S. and Morris, M. (1999) \emph{Relative Distribution
    Methods in the Social Sciences}.  Springer-Verlag.  \acronym{ISBN}:
  0387987789.
}
\author{ Mikko Korpela, based on original by Andy Bunn }
\seealso{ \code{\link{rwl.stats}} }
\examples{data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prefix = "CAM")
gini.coef(ca533.crn)
}
\keyword{ univar }
