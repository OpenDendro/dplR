\encoding{UTF-8}
\name{chron.stabilized}
\alias{chron.stabilized}
\title{Build Mean Value Chronology with Stabilized Variance}
\description{
  This function builds a mean value chronology, typically from a
  \code{data.frame} of detrended ring widths as produced by
  \code{\link{detrend}}.
}
\usage{
chron.stabilized(x, winLength=51,biweight=TRUE, running.rbar = FALSE, \dots)
}
\arguments{
  \item{x}{a \code{data.frame} of ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(x)}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{winLength}{a odd \code{integer} specifiying the window length.}
  \item{biweight}{\code{logical} flag.  If \acronym{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}.}}
  \item{running.rbar}{\code{logical} flag.  If \acronym{TRUE} the running interseries correlation is returned as well. }
}
\details{
  Here.
}
\value{
  A \code{data.frame} with the variance stabilized chronology, running interseries correlation (`if \code{running.bar=TRUE}), and the sample depth.
}
\references{
  Add Frank refs from TRACE and GRL.
  
  Cook, E. R. and Kairiukstis, L. A., editors (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
    Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.  Blackburn.
  \acronym{ISBN-13}: 978-1-930665-39-2.
}
\author{ Stefan Klesse.  Patched and improved by Andy Bunn. 
}
\seealso{ \code{\link{chron}} 
}
\examples{library(graphics)
library(utils)
data(co021)
co021.rwi <- detrend(co021,method = "Spline")
co021.crn <- chron(co021.rwi)
co021.crn2 <- stabilized.chron(co021.rwi,
                                winLength=101,
                                biweight = TRUE,
                                running.rbar = FALSE)
yrs <- time(co021)
plot(yrs,co021.crn$xxxstd,type="l",col="grey")
lines(yrs,co021.crn2$adj.crn,col="red")
}
\keyword{ manip }
