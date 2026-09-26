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
\details{
  Each segment is drawn in blue if it correlates above the critical
  value, red if it does not, and green where it does not overlap the bin
  completely.  If \code{\link{corr.rwl.seg}} was run with
  \code{\var{lag.max}} greater than 0, segments that correlate better at
  another lag (COFECHA\enc{’}{'}s B flag) are drawn in purple instead,
  whatever their correlation as dated.
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
