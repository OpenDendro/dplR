\name{chron}
\alias{chron}
\title{Build Mean Value Chronology}
\description{
  This function builds a mean value chronology, typically from a
  \code{data.frame} of detrended ring widths as produced by
  \code{\link{detrend}}.
}
\usage{
chron(x, prefix = "xxx", biweight = TRUE, prewhiten = FALSE)
}
\arguments{
  \item{x}{a \code{data.frame} of ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(x)}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{prefix}{a \code{character} string with less than 4
    characters. Defaults to \code{"xxx"}}
  \item{biweight}{\code{logical} flag. If \acronym{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}.}}
  \item{prewhiten}{\code{logical} flag. If \acronym{TRUE} each series is
    whitened using \code{\link{ar}} prior to averaging.}
}
\details{
  This either averages the rows of the \code{data.frame} using a mean or
  a robust mean (the so-called standard chronology) or can do so from
  the residuals of an ar process (the residual chronology).
}
\value{
  A \code{data.frame} with the standard chronology, residual chronology (if
  prewhitening was performed), and the sample depth.
}
\references{
  Cook, E. R. and Kairiukstis, L. A. (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences.}
  Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate.}  Blackburn.
  \acronym{ISBN-13}: 978-1-930665-39-2.
}
\author{ Andy Bunn. Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{read.rwl}}, \code{\link{detrend}},
  \code{\link{ar}}, \code{\link{chron}}
}
\examples{data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi, prefix = "CAM")
## With residual chron
ca533.crn <- chron(ca533.rwi, prefix = "CAM", prewhiten = TRUE)
}
\keyword{ manip }
