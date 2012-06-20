\name{dplR-package}
\alias{dplR-package}
\alias{dplR}
\docType{package}
\title{
  Dendrochronology Program Library in \R
}
\description{
  This package contains functions for performing some standard tree-ring
  analyses.
}
\details{
\tabular{ll}{
Package: \tab dplR\cr
Type: \tab Package\cr
License: \tab \acronym{GPL}\cr
}
\emph{Main Functions}
  \describe{
    \item{\code{\link{read.rwl}}}{ reads rwl files }
    \item{\code{\link{detrend}}}{ detrends raw ring widths }
    \item{\code{\link{chron}}}{ builds chronologies }
    \item{\code{\link{corr.rwl.seg}}}{ crossdating function }
  }
}
\author{
  Andy Bunn \email{andy.bunn@wwu.edu} with major additions from Mikko
  Korpela and other significant contributions from Franco Biondi, Pierre
  \enc{Mérian}{Merian}, Fares Qeadan and Christian Zang
}
\references{
  Cook, E. R. and Kairiukstis, L. A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences.}
  Springer.  \acronym{ISBN-13}: 978-0792305866.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate.}
  Blackburn.  \acronym{ISBN-13}: 978-1930665392.
}
\keyword{ package }
