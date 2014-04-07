\name{plot.rwl}
\alias{plot.rwl}
\title{
  Plotting rwl objects
}
\description{
  Plots rwl objects
}
\usage{
\method{plot}{rwl}(x, plot.type=c("seg","spag"), ...)
}
\arguments{

  \item{x}{ An object of class \code{"rwl"}.  }

  \item{plot.type}{ Character. Type "seg" calls \code{\link{seg.plot}} 
  while "spag" calls \code{\link{spag.plot}} }

  \item{\dots}{ Additional arguemnts for each \code{type}  }
  
}
\value{
  None. A plot is produced.
}
\author{
  Andy Bunn
}
\seealso{
  \code{\link{read.rwl}}
}
\examples{data(co021)
plot(co021, plot.type="seg")
plot(co021, plot.type="spag")
plot(co021, plot.type="spag", zfac=2)
}
\keyword{ hplot }
