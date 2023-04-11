\encoding{UTF-8}
\name{plot.crn}
\alias{plot.crn}
\alias{crn.plot}
\title{ Plot a Tree-Ring Chronology }
\description{
  This function makes a default plot of a tree-ring chronology from a
  \code{data.frame} of the type produced by \code{\link{chron}}, 
  \code{\link{chron.ars}}, \code{\link{chron.stabilized}}, \code{\link{ssf}}.
}
\usage{
\method{plot}{crn}(x, add.spline = FALSE, nyrs = NULL, ...)

}
\arguments{
  \item{x}{ a \code{data.frame} of \code{class{crn}}.e.g., as produced by
    \code{\link{chron}}, \code{\link{chron.ars}}, \code{\link{chron.stabilized}}, 
    \code{\link{ssf}}. }
  \item{add.spline}{ a \code{logical} flag.  Will add a line with a
    smoothing spline using \code{\link{caps} } }
  \item{nyrs}{ a number giving the rigidity of the smoothing spline.
    Defaults to 1/3 times the length of the first chronology if
    \code{\var{nyrs}} is \code{NULL} }
  \item{\dots}{ Additional arguments to pass to \code{\link{plot}}. }
}
\details{
  This makes a crude plot of one or more tree-ring chronologies.
}
\value{
  None.  Invoked for side effect (plot).
}
\author{ Andy Bunn.  Patched and improved by Mikko Korpela. }
\seealso{ \code{\link{chron}}
}
\examples{library(graphics)
data(wa082)
# Truncate the RW data to a sample depth at least 5
wa082Trunc <- wa082[rowSums(!is.na(wa082))>4,]
# Detrend with age-dependent spline
wa082RWI <- detrend(wa082Trunc,method = "AgeDep")
# make several chronologies
wa082CRN1 <- chron(wa082RWI)
wa082CRN2 <- chron.stabilized(wa082RWI,
                              winLength=51,
                              biweight = TRUE,
                              running.rbar = TRUE)
wa082CRN3 <- chron.ars(wa082RWI)
wa082CRN4 <- ssf(wa082Trunc)

# and plot
plot.crn(wa082CRN1,add.spline = TRUE,nyrs=20)
plot.crn(wa082CRN2,add.spline = TRUE,nyrs=20)
plot(wa082CRN3,add.spline = TRUE,nyrs=20)
plot(wa082CRN4,add.spline = TRUE,nyrs=20)

# a custom crn
foo <- data.frame(wa082CRN1,sfc=wa082CRN4$sfc)
foo <- foo[,c(1,3,2)]
class(foo) <- c("crn","data.frame")
plot.crn(foo,add.spline = TRUE,nyrs=20)

}
\keyword{ hplot }

