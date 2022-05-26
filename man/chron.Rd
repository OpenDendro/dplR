\encoding{UTF-8}
\name{chron}
\alias{chron}
\title{Build Mean Value Chronology}
\description{
  This function builds a mean value chronology, typically from a
  \code{data.frame} of detrended ring widths as produced by
  \code{\link{detrend}}.
}
\usage{
chron(x, biweight = TRUE, prewhiten = FALSE, \dots)
}
\arguments{
  \item{x}{a \code{data.frame} of (usually detrended) ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(x)}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{biweight}{\code{logical} flag.  If \acronym{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}.}}
  \item{prewhiten}{\code{logical} flag.  If \acronym{TRUE} each series is
    whitened using \code{\link{ar}} prior to averaging.}
  \item{\dots}{Arguments passed to \code{\link{ar}} when
    \code{prewhiten} is \code{TRUE}.  For example, use \code{aic}
    and \code{order.max} to control the order of the AR model.}
}
\details{
  This either averages the rows of the \code{data.frame} using a mean or
  a robust mean (the so-called standard chronology) or can do so from
  the residuals of an AR process (the residual chronology).
  
  Note that the residual chronology in this function will return different
  values than the residual chronology from \code{\link{chron.ars}} which uses
  a slightly different method for determining AR order.
}
\value{
  An object of of class \code{crn} and \code{data.frame} with the standard chronology, residual chronology (if prewhitening was performed), and the sample depth. The years are stored as row numbers.
}
\references{
  Cook, E. R. and Kairiukstis, L. A., editors (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
    Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.  Blackburn.
  \acronym{ISBN-13}: 978-1-930665-39-2.
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{read.rwl}}, \code{\link{detrend}},
  \code{\link{ar}}, \code{\link{crn.plot}}
}
\examples{library(graphics)
library(utils)
data(ca533)
ca533.rwi <- detrend(rwl = ca533, method = "ModNegExp")
ca533.crn <- chron(ca533.rwi)
plot(ca533.crn,xlab="Year",ylab="RWI")
## With residual chron
ca533.crn2 <- chron(ca533.rwi, prewhiten = TRUE)
plot(ca533.crn2,xlab="Year",ylab="RWI")
}
\keyword{ manip }
