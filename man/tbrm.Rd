\name{tbrm}
\alias{tbrm}
\title{ Calculate Tukey's Biweight Robust Mean }
\description{
  This calculates a robust average that is unaffected by outliers.
}
\usage{
tbrm(x, C = 9)
}
\arguments{
  \item{x}{ a \code{numeric} vector }
  \item{C}{ a constant. \code{\var{C}} is preassigned a value of 9
    according to the Cook reference below but other values are
    possible. }
}
\details{
  This is a one step computation that follows the Affy whitepaper below,
  see page 22. This function is called by \code{\link{chron}} to
  calculate a robust mean.  \code{\var{C}} determines the point at which
  outliers are given a weight of 0 and therefore do not contribute to
  the calculation of the mean.  \code{\var{C} = 9} sets values roughly
  +/-6 standard deviations to 0. \code{\var{C} = 6} is also used in
  tree-ring chronology development. Cook and Kairiukstis (1990) have
  further details.

  An exact summation algorithm (Shewchuk 1997) is used. When some
  assumptions about the rounding of floating point numbers and
  conservative compiler optimizations hold, summation error is
  completely avoided.  Whether the assumptions hold depends on the
  platform, i.e. compiler and \acronym{CPU}.
}
\value{
  A \code{numeric} mean.
}
\references{

  Statistical Algorithms Description Document, 2002, Affymetrix.

  Cook, E. R. and Kairiukstis, L. A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences}.
  Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Mosteller, F. and Tukey, J. W. (1977) \emph{Data Analysis and
  Regression: a second course in statistics}.  Addison-Wesley.
  \acronym{ISBN-13}: 978-0-201-04854-4.

  Shewchuk, J. R. (1997) Adaptive Precision Floating-Point Arithmetic
  and Fast Robust Geometric Predicates.  \emph{Discrete and
  Computational Geometry}, 18(3):305\enc{–}{--}363. Springer.

}
\author{ Mikko Korpela }
\seealso{ \code{\link{chron}} }
\examples{tbrm(rnorm(100))

## Compare
data(co021)
co021.rwi <- detrend(co021, method = "ModNegExp")
crn1 <- apply(co021.rwi, 1, tbrm)
crn2 <- chron(co021.rwi)
cor(crn1, crn2[, 1])
}
\keyword{ robust }
\keyword{ univar }
