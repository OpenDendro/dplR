\name{chron}
\alias{chron}
\title{Build Mean Value Chronology}
\description{
  This function builds a mean value chronology, typically from a \code{data.frame}
  of detrended ring widths as produced by \code{\link{detrend}}.
}
\usage{
chron(x, prefix = NULL, biweight = TRUE, prewhiten = FALSE)
}
\arguments{
  \item{x}{a \code{data.frame} of ring widths with rownames(x) containing years
    and colnames(x) containing each series id such as produced by
    \code{\link{read.rwl}}}
  \item{prefix}{a character vector with a length of < 4. Defaults to xxx}
  \item{biweight}{logical flag. If TRUE then a robust mean is calculated
    using \code{\link{tbrm}.}}
  \item{prewhiten}{logical flag. If TRUE each series is whitened using
      \code{\link[stats]{ar}} prior to averaging.}
}
\details{
  This either averages the rows of the \code{data.frame} using a mean or a
  robust mean (the so-called standard chronology) or can do so from the
  residuals of an ar process (the residual chronology).
}
\value{
  A \code{data.frame} with the standard chronology, redisual chronology (if
  prewhitening was performed), and the sample depth.
}
\references{ Cook, E. R. and Kairiukstis, L.A. (1990) \emph{Methods of
  Dendrochronology: Applications in the Environmental Sciences.} Springer.
  ISBN-13: 978-0792305866.

  Fritts, H.C. (2001) \emph{Tree Rings and Climate.} Blackburn. ISBN-13:
  978-1930665392.
}
\author{ Andy Bunn }
\seealso{ \code{\link{read.rwl}}, \code{\link{detrend}},
  \code{\link[stats]{ar}}, \code{\link{chron}}
}
\examples{

  data(ca533)
  ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
  ca533.crn <- chron(ca533.rwi, prefix = "CAM")
  # With residual chron
  ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = TRUE)

}
\keyword{ manip }
