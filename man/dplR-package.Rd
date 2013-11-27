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
  Korpela and other significant contributions from Franco Biondi, Filipe
  Campelo, Pierre \enc{Mérian}{Merian}, Fares Qeadan and Christian
  Zang.  Function \code{\link{redfit}} is an improved translation of
  program REDFIT which is original work of Manfred Mudelsee and Michael
  Schulz.  Jacob Cecile contributed a bug fix to
  \code{\link{detrend.series}}.
}
\references{
  Cook, E. R. and Kairiukstis, L. A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences.}
  Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate.}
  Blackburn.  \acronym{ISBN-13}: 978-1-930665-39-2.
}
\keyword{ package }
