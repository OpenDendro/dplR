\encoding{UTF-8}
\name{chron.stabilized}
\alias{chron.stabilized}
\title{Build Mean Value Chronology with Stabilized Variance}
\description{
  This function builds a variance stabilized mean-value chronology, typically from a
  \code{data.frame} of detrended ring widths as produced by
  \code{\link{detrend}}.
}
\usage{
chron.stabilized(x, winLength, biweight = TRUE, running.rbar = FALSE)
}
\arguments{
  \item{x}{a \code{data.frame} of ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(x)}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{winLength}{a odd \code{integer} specifying the window length.}
  \item{biweight}{\code{logical} flag.  If \acronym{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}.}}
  \item{running.rbar}{\code{logical} flag.  If \acronym{TRUE} the running interseries correlation is returned as well. }
}
\details{
  The variance of a mean chronology depends on the variance of the individual samples, the number of series averaged together, and their interseries correlation (Wigley et al. 1984). As the number of series commonly decreases towards the beginning of a chronology averaging introduces changes in variance that are a solely an effect of changes in sample depth.
  
  Additionally, time-dependent changes in interseries correlation can cause artificial variance changes of the final mean chronology. The function \code{chron.stabilized} accounts for both temporal changes in the interseries correlation and sample depth to produce a mean value chronology with stabilized variance.
  
  The basic correction centers around the use of the effective independent sample size, \code{Neff}, which considers sample replication and mean interseries correlation between the samples at every time. This is defined as: \code{Neff = n(t) / 1+(n(t)-1)rbar(t)}

where \code{n(t)} is the number of series at time \code{t}, and \code{rbar} is the interseries correlation (see \code{\link{interseries.cor}}). Multiplication of the mean time series with the square root of \code{Neff} at every time \code{t} theoretically results in variance that is independent of sample size. In the limiting cases, when the \code{rbar} is zero or unity, \code{Neff} obtains values of the true sample size and unity, respectively.


}
\value{
  A \code{data.frame} with the variance stabilized chronology, running interseries correlation (`if \code{running.bar=TRUE}), and the sample depth.
}
\references{
  Frank, D, Esper, J, Cook, E, (2006) \emph{On variance adjustments in tree-ring chronology development}. Tree rings in archaeology, climatology and ecology, TRACE 4, 56–66
  
  Frank, D, Esper, J, Cook, E, (2007) \emph{Adjustment for proxy number and coherence in a large-scale temperature reconstruction}. Geophysical Research Letters 34
  
  Wigley, T, Briffa K, Jones P (1984) \emph{On the Average Value of Correlated Time Series, with Applications in Dendroclimatology and Hydrometeorology}. J. Climate Appl. Meteor., 23, 201–213
  }
\author{ Original code by David Frank and adapted for dplR by Stefan Klesse. Patched and improved by Andy Bunn. 
}
\seealso{ \code{\link{chron}} 
}
\examples{library(graphics)
library(utils)
data(co021)
co021.rwi <- detrend(co021,method = "Spline")
co021.crn <- chron(co021.rwi)
co021.crn2 <- chron.stabilized(co021.rwi,
                                winLength=101,
                                biweight = TRUE,
                                running.rbar = FALSE)
yrs <- time(co021)
plot(yrs,co021.crn$std,type="l",col="grey")
lines(yrs,co021.crn2$adj.crn,col="red")
}
\keyword{ manip }
