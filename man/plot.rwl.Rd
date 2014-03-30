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
\examples{data(ca533)
## to use as S3Method the class of rwl must be set:
class(ca533) <- c('rwl','data.frame')
plot(ca533,plot.type=c('seg'))
plot(ca533,plot.type=c('spag'))
plot(ca533,plot.type=c('spag'),zfac=2)
}
\keyword{ plot }
