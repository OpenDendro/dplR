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
    Cook reference below but other values are possible. }
}
\details{
  This is a one step computation that follows the Affy whitepaper below see
  page 22. This function is called by  \code{\link{chron}} to calculate a
  robust mean.  \code{C} determines the point at which outliers are given a
  weight of 0 and therefore do not contribute to the calculation of the mean.
  C=9 sets values roughly +/-6 standard deviations to 0. C=6 is also used in
  tree-ring chronology development. Cook and Kairiukstis (1990) have further
  details.

  An exact summation algorithm (Shewchuk 1997) is used. When some assumptions 
  about the rounding of floating point numbers and conservative compiler 
  optimizations hold, summation error is completely avoided. 
  Whether the assumptions hold depends on the platform, i.e. compiler and CPU.
  
}
\value{
  A numeric mean.
}
\references{

  Cook, E. R. and Kairiukstis, L.A. (1990) \emph{Methods of Dendrochronology:
  Applications in the Environmental Sciences.} Springer.
  ISBN-13: 978-0792305866.

  Mosteller, F. and Tukey, J. W. (1977) \emph{Data Analysis and Regression:
  a second course in statistics.} Addison-Wesley. ISBN-13: 978-0201048544.

  Shewchuk, J. R. (1997) \emph{Adaptive Precision Floating-Point
  Arithmetic and Fast Robust Geometric Predicates.} Discrete and
  Computational Geometry, 18(3):305--363. Springer.
  
}
\author{ Mikko Korpela }
\seealso{ \code{\link{chron}} }
\examples{
tbrm(rnorm(100))

## Compare
data(co021)
co021.rwi <- detrend(co021, method = "ModNegExp")
crn1 <- apply(co021.rwi, 1, tbrm)
crn2 <- chron(co021.rwi)
cor(crn1, crn2[, 1])
}
\keyword{ misc }
