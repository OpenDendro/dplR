\encoding{UTF-8}
\name{chron.ars}
\alias{chron.ars}
\title{Build Mean Value Chronology}
\description{
  This function builds three varieties of a mean value chronology, typically from a
  \code{data.frame} of detrended ring widths as produced by
  \code{\link{detrend}}.
}
\usage{
chron.ars(x, biweight=TRUE, maxLag=10, firstAICmin=TRUE, verbose = TRUE)
}
\arguments{
  \item{x}{a \code{data.frame} of (usually detrended) ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(x)}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{biweight}{\code{logical} flag.  If \acronym{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}.}}
  \item{firstAICmin}{\code{logical} flag.  If \acronym{TRUE} each series is
    whitened using \code{\link{ar}} prior to averaging.}
  \item{verbose}{\code{logical} flag.  If \acronym{TRUE} the function is chatty.}
}
\details{
  This produces three mean value chronnologies: standard, residual, and ARSTAN. More details to follow but important to note that this is different than the existing chron function in terms of the way the residual chron is calculated and the ARSTAN chron is new.
}
\value{
  A \code{data.frame} with the standard, residual, and ARSTAN chronologies. The sample depth is also included.
}
\references{
  Cook, E. R. and Kairiukstis, L. A., editors (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
    Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.  Blackburn.
  \acronym{ISBN-13}: 978-1-930665-39-2.
}
\author{ Andy Bunn. Add in KJA and Ed Cook. }
\seealso{ \code{\link{chron}}, \code{\link{detrend}},
  \code{\link{ar}}, \code{\link{crn.plot}}
}
\examples{library(graphics)
library(utils)
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "AgeDepSpline")
ca533.crn <- chron.ars(ca533.rwi)
plot(ca533.crn,xlab="Year",ylab="RWI")
}
\keyword{ manip }
