\encoding{UTF-8}
\name{plot.crs}
\alias{plot.crs}
\title{
  Plotting crs Objects
}
\description{
  Plots objects returned from \code{\link{corr.rwl.seg}}.
}
\usage{
\method{plot}{crs}(x, ...)
}
\arguments{

  \item{x}{ An object of class \code{"crs"}.  }

  \item{\dots}{ Additional arguments passed to \code{\link{plot}}  }
  
}
\value{
  None.  A plot is produced.
}
\author{
  Andy Bunn
}
\seealso{
  \code{\link{corr.rwl.seg}}
}
\examples{
library(graphics)
data(co021)
foo <- corr.rwl.seg(co021, make.plot = FALSE)
plot(foo)
}
\keyword{ hplot }
