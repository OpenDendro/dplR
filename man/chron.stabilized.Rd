\encoding{UTF-8}
\name{chron.stabilized}
\alias{chron.stabilized}
\title{Build Mean Value Chronology with Stabilized Variance}
\description{
  This function builds a variance stabilized mean value chronology, typically from a
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
  The variance of a mean chronology depends on the variance of the individual samples, the number of series averaged together, and their interseries correlation (Wigley et al. 1984). As the number of series commonly decreases towards the beginning of a chronology averaging introduces changes in variance that are a solely an effect of changes in sample replication.
  Additionally, time-dependent changes in interseries correlation can also cause artificial variance changes of the final mean chronology. The function \code{chron.stabilized} accounts for both temporal changes in rbar and sample replication.
  
  The basic correction centers around the use of the effective independent sample size, Neff, which considers sample replication and mean interseries correlation between the samples at every time. This is defined as:
Neff = n(t) / 1+(n(t)−1)r(t)
where n(t) is the number of series at time t, and r is rbar. Multiplication of the mean timeseries with the square root of Neff at every time t theoretically results in variance that is independent of sample size. This result can then be further scaled by the square root of 1/rbar to yield an estimate of the mean series in the original units. In the limiting cases, when the rbar is zero or unity, Neff obtains values of the true sample size and unity, respectively.
}
\value{
  A \code{data.frame} with the variance stabilized chronology, running interseries correlation (`if \code{running.bar=TRUE}), and the sample depth.
}
\references{
  Add Frank refs from TRACE and GRL.
  
  Frank, D., Esper, J., Cook, E.R., (2006) \emph{On variance adjustments in tree-ring chronology development}. Tree rings in archaeology, climatology and ecology, TRACE 4, 56–66.
  
  Frank, D., Esper, J., Cook, E.R., (2007) \emph{Adjustment for proxy number and coherence in a large-scale temperature reconstruction}. Geophysical Research Letters 34, https://doi.org/10.1029/2007GL030571
  
  Cook, E. R. and Kairiukstis, L. A., editors (1990) \emph{Methods of
    Dendrochronology: Applications in the Environmental Sciences}.
    Springer.  \acronym{ISBN-13}: 978-0-7923-0586-6.

  Fritts, H. C. (2001) \emph{Tree Rings and Climate}.  Blackburn.
  \acronym{ISBN-13}: 978-1-930665-39-2.
}
\author{ David Frank. Patched and improved by Andy Bunn and Stefan Klesse. 
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
