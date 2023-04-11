\encoding{UTF-8}
\name{chron.ci}
\alias{chron.ci}
\title{Build Mean Value Chronology with Confidence Intervals}
\description{
  This function builds a mean value chronology with bootstrapped confidence 
  intervals, typically from a \code{data.frame} of detrended ring widths as produced by
  \code{\link{detrend}}.
}
\usage{
chron.ci(x, biweight=TRUE, conf=0.95, R=100)
}
\arguments{
  \item{x}{a \code{data.frame} of (usually detrended) ring widths with
    \code{rownames(\var{x})} containing years and \code{colnames(x)}
    containing each series \acronym{ID} such as produced by
    \code{\link{read.rwl}}}
  \item{biweight}{\code{logical} flag.  If \acronym{TRUE} then a robust
    mean is calculated using \code{\link{tbrm}.}}
  \item{conf}{\code{numeric}. A scalar for the confidence level.}
  \item{R}{\code{integer}. The number of bootstrap replicates. }
}
\details{
  This either averages the rows of the \code{data.frame} using a mean or
  a robust mean (the so-called standard chronology) and calculates boostrapped confidence intervals using the normal approximation. The function will fail if there are any rows in \code{x} that contain only one sample and in practice there should be several samples in a row. One of the guiding principles of bootstrapping is that the population is to the sample as the sample is to the bootstrap samples.
}
\value{
  An object of of class \code{data.frame} with the standard chronology, the upper and lower confidence interval, and the sample depth. The years are stored as row numbers.
}

\author{ Andy Bunn.}
\seealso{ \code{\link{read.rwl}}, \code{\link{detrend}},
  \code{\link{boot}}, \code{\link{boot.ci}}
}
\examples{library(graphics)
library(utils)
data(wa082)
# truncate to a sample depth of five
wa082Trunc <- wa082[rowSums(!is.na(wa082))>4,]
# detrend
wa082RWI <- detrend(wa082Trunc, method="AgeDepSpline")
# bootstrap the chronology and
wa082Crn <- chron.ci(wa082RWI, biweight = TRUE, R = 100, conf = 0.99)
head(wa082Crn)
# plot (this is so much easier in ggplot!)
wa082Crn$yrs <- time(wa082Crn)
xx <- c(wa082Crn$yrs,rev(wa082Crn$yrs))
yy <- c(wa082Crn$lowerCI,rev(wa082Crn$upperCI))
plot(wa082Crn$yrs,wa082Crn$std,type="n",ylim=range(yy),
     ylab="RWI",xlab="Year",main="Chronology with CI")
polygon(x=xx,y=yy,col = "grey",border = NA)
lines(wa082Crn$yrs,wa082Crn$std)
}
\keyword{ manip }
