\name{tbrm}
\alias{tbrm}
\title{ Calculates Tukey's Biweight Robust Mean }
\description{
  This calculates a robust average that is unaffected by outliers.
}
\usage{
tbrm(x, C = 9)
}
\arguments{
  \item{x}{ a numeric vector }
  \item{C}{ a constant. \code{C} is preassigned a value of 9 according to the
    Cook reference below. }
}
\details{
  This is a one step computation that follows the Affy whitepaper below see
  page 22. This function is called by  \code{\link{chron}} to calculate a
  robust mean. Cook and Kairiukstis (1990) have further details.
}
\value{
  A numeric mean.
}
\references{ Statistical Algorithms Description Document, 2002, Affymetrix. p22.
  Cook, E. R. and Kairiukstis, L.A. (1990) \emph{Methods of Dendrochronology:
  Applications in the Environmental Sciences.} Springer.
  ISBN-13: 978-0792305866. }
\author{ Andy Bunn }
\seealso{ \code{\link{chron}} }
\examples{

  tbrm(rnorm(100))

  # Compare
  data(co021)
  co021.rwi <- detrend(co021,method = "Spline")
  crn1 <- apply(co021.rwi,1,tbrm)
  crn2 <- chron(co021.rwi)
  cor(crn1,crn2[,1])

}
\keyword{ misc }
