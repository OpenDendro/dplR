\encoding{UTF-8}
\name{chron.ars}
\alias{chron.ars}
\title{Build ARSTAN Chronology}
\description{
  This function builds three varieties of the mean value chronology, including 
  the ARSTAN chronology, typically from a
  \code{data.frame} of detrended ring widths as produced by
  \code{\link{detrend}}.
}
\usage{
chron.ars(x, biweight=TRUE, maxLag=10, firstAICmin=TRUE, verbose=TRUE)
}
\arguments{
  \item{x}{a \code{data.frame} of (usually detrended) ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(x)}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{biweight}{\code{logical} flag.  If \acronym{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}.}}
  \item{maxLag}{an \code{integer} giving the maximum lag to consider in the AR pooling.}  
  \item{firstAICmin}{\code{logical} flag.  If \acronym{TRUE} each series is
    whitened using \code{\link{ar}} prior to averaging.}
  \item{verbose}{\code{logical} flag.  If \acronym{TRUE} the function is chatty.}
}
\details{
  This produces three mean value chronnologies: standard, residual, and ARSTAN. 
  
  It's important to note that residual chronology produced here is different than the simple
  residual chronology produced by \code{\link{chron}}.See pp 153-154 in Cook's dissertation.
}
\value{
  A \code{data.frame} with the standard, residual, and ARSTAN chronologies. The sample depth is also included.
}
\references{
  Cook, E. R. and Kairiukstis, L. A., editors (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
    Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Cook, E. R. (1985). A Time Series Analysis Approach to Tree Ring 
    Standardization. PhD thesis, The University of Arizona.
}
\author{ Andy Bunn with contributions from Kevin Achukaitis and Ed Cook. }
\seealso{ \code{\link{chron}}, \code{\link{crn.plot}} }
}
\examples{library(graphics)
library(utils)
data(wa082)
wa082.rwi <- detrend(rwl = wa082, method = "AgeDepSpline")
wa082.crn <- chron.ars(wa082.rwi)
plot(wa082.crn,xlab="Year",ylab="RWI",add.spline=TRUE,nyrs=20)
}
\keyword{ manip }
